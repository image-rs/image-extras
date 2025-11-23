//! Decoding and encoding DDS images
//!
//! DDS (DirectDraw Surface) is a container format for storing DXT (S3TC) compressed images.
//!
//! # Related Links
//!
//! * <https://docs.microsoft.com/en-us/windows/win32/direct3ddds/dx-graphics-dds-pguide> - Description of the DDS format.

use std::io::{Read, Seek};

use dds::header::ParseOptions;
use dds::{
    Channels, ColorFormat, DataLayout, Decoder, Format, ImageViewMut, Offset, Size,
    TextureArrayKind,
};

use image::error::{
    DecodingError, ImageError, ImageFormatHint, ImageResult, LimitError, LimitErrorKind,
    UnsupportedError, UnsupportedErrorKind,
};
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageDecoderRect};

/// DDS decoder.
///
/// This decoder supports decoding DDS files with a single texture, including
/// cube maps. Texture arrays and volumes are not supported.
///
/// It's possible to set the color type the image is decoded as using
/// [`DdsDecoder::set_color_type`].
pub struct DdsDecoder<R: Read> {
    inner: Decoder<R>,
    is_cubemap: bool,
    size: Size,
    color: SupportedColor,
}

impl<R: Read> DdsDecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(r: R) -> ImageResult<Self> {
        let options = ParseOptions::new_permissive(None);
        let decoder = Decoder::new_with_options(r, &options).map_err(to_image_error)?;
        let layout = decoder.layout();

        // We only support DDS files with:
        // - A single main image with any number of mipmaps
        // - A texture array of length 1 representing a cube map
        match &layout {
            DataLayout::Volume(_) => {
                return Err(ImageError::Decoding(DecodingError::new(
                    format_hint(),
                    "DDS volume textures are not supported for decoding",
                )))
            }
            DataLayout::TextureArray(texture_array) => {
                let supported_length = match texture_array.kind() {
                    TextureArrayKind::Textures => 1,
                    TextureArrayKind::CubeMaps => 6,
                    TextureArrayKind::PartialCubeMap(cube_map_faces) => cube_map_faces.count(),
                };
                if texture_array.len() != supported_length as usize {
                    return Err(ImageError::Decoding(DecodingError::new(
                        format_hint(),
                        "DDS texture arrays are not supported for decoding",
                    )));
                }
            }
            DataLayout::Texture(_) => {}
        }

        let mut size = decoder.main_size();
        let mut color = decoder.native_color();
        let is_cubemap = layout.is_cube_map();

        // all cube map faces are read as one RGBA image
        if is_cubemap {
            if let (Some(width), Some(height)) =
                (size.width.checked_mul(4), size.height.checked_mul(3))
            {
                size.width = width;
                size.height = height;
                color.channels = Channels::Rgba;
            } else {
                return Err(ImageError::Decoding(DecodingError::new(
                    format_hint(),
                    "DDS cube map faces are too large to decode",
                )));
            }
        }

        Ok(DdsDecoder {
            inner: decoder,
            is_cubemap,
            size,
            color: SupportedColor::from_dds_widen(color),
        })
    }

    /// Set the color type for the decoder.
    ///
    /// The DDS decoder supports decoding images not just in their native color
    /// format, but any user-defined color format. This is useful for decoding
    /// images that do not cleanly fit into the native formats. E.g. the DDS
    /// format `B5G6R5_UNORM` is decoded as [`ColorType::Rgb8`] by default, but
    /// you may want to decode it as [`ColorType::Rgb32F`] instead to avoid the
    /// rounding error when converting to `u8`. Similarly, your application may
    /// only support 8-bit images, while the DDS file is in a 16/32-bit format.
    /// Decoding directly into the final color type is more efficient than
    /// decoding into the native format and then converting.
    ///
    /// # Errors
    ///
    /// Currently, [`ColorType::La8`] and [`ColorType::La16`] are not supported
    /// for decoding DDS files. If these color types (or other unsupported types)
    /// are provided, this function will return [`ImageError::Unsupported`] with
    /// [`UnsupportedErrorKind::Color`].
    #[track_caller]
    pub fn set_color_type(&mut self, color: ColorType) -> ImageResult<()> {
        let Some(supported_color) = SupportedColor::from_image_exact(color) else {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    format_hint(),
                    UnsupportedErrorKind::Color(color.into()),
                ),
            ));
        };
        self.color = supported_color;
        Ok(())
    }
}

impl<R: Read + Seek> ImageDecoder for DdsDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.size.width, self.size.height)
    }

    fn color_type(&self) -> ColorType {
        self.color.image
    }

    fn original_color_type(&self) -> ExtendedColorType {
        match self.inner.format() {
            Format::R1_UNORM => ExtendedColorType::L1,
            Format::B4G4R4A4_UNORM | Format::A4B4G4R4_UNORM => ExtendedColorType::Rgba4,
            Format::A8_UNORM => ExtendedColorType::A8,
            _ => SupportedColor::from_dds_widen(self.inner.native_color())
                .image
                .into(),
        }
    }

    fn set_limits(&mut self, limits: image::Limits) -> ImageResult<()> {
        limits.check_dimensions(self.size.width, self.size.height)?;

        if let Some(max_alloc) = limits.max_alloc {
            self.inner.options.memory_limit = max_alloc.try_into().unwrap_or(usize::MAX);
        }

        Ok(())
    }

    #[track_caller]
    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        let color = self.color.dds;
        let size = self.size;

        assert_eq!(
            buf.len(),
            color.buffer_size(size).unwrap(),
            "Buffer len does not match for {:?} and {:?}",
            size,
            color
        );

        let image = ImageViewMut::new(buf, size, color).expect("Invalid buffer length");

        if self.is_cubemap {
            self.inner.read_cube_map(image).map_err(to_image_error)?;
        } else {
            self.inner.read_surface(image).map_err(to_image_error)?;
        }

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

impl<R: Read + Seek> ImageDecoderRect for DdsDecoder<R> {
    fn read_rect(
        &mut self,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
        buf: &mut [u8],
        row_pitch: usize,
    ) -> ImageResult<()> {
        // reading rectangles is not supported for cube maps
        if self.is_cubemap {
            return Err(ImageError::Decoding(DecodingError::new(
                format_hint(),
                "read_rect is not supported for DDS cubemaps",
            )));
        }

        let Some(view) =
            ImageViewMut::new_with(buf, row_pitch, Size::new(width, height), self.color.dds)
        else {
            return Err(ImageError::Decoding(DecodingError::new(
                format_hint(),
                "Invalid buffer length for reading rect",
            )));
        };

        self.inner
            .read_surface_rect(view, Offset::new(x, y))
            .map_err(to_image_error)?;
        self.inner
            .rewind_to_previous_surface()
            .map_err(to_image_error)?;

        Ok(())
    }
}

/// A color type supported by both the `image` and `dds` crates.
struct SupportedColor {
    image: ColorType,
    dds: ColorFormat,
}
impl SupportedColor {
    fn new(image: ColorType, dds: ColorFormat) -> SupportedColor {
        debug_assert_eq!(image.bytes_per_pixel(), dds.bytes_per_pixel());
        debug_assert_eq!(image.channel_count(), dds.channels.count());

        SupportedColor { image, dds }
    }

    /// Returns a supported color format that exactly matches the given `image`
    /// color format.
    fn from_image_exact(color: ColorType) -> Option<Self> {
        fn to_color_format_exact(color: ColorType) -> Option<ColorFormat> {
            match color {
                ColorType::L8 => Some(ColorFormat::GRAYSCALE_U8),
                ColorType::Rgb8 => Some(ColorFormat::RGB_U8),
                ColorType::Rgba8 => Some(ColorFormat::RGBA_U8),
                ColorType::L16 => Some(ColorFormat::GRAYSCALE_U16),
                ColorType::Rgb16 => Some(ColorFormat::RGB_U16),
                ColorType::Rgba16 => Some(ColorFormat::RGBA_U16),
                ColorType::Rgb32F => Some(ColorFormat::RGB_F32),
                ColorType::Rgba32F => Some(ColorFormat::RGBA_F32),
                _ => None,
            }
        }

        to_color_format_exact(color).map(|dds| Self::new(color, dds))
    }
    /// Returns a supported color format that is the narrowest superset of the
    /// given `image` color format.
    fn from_dds_widen(color: ColorFormat) -> Self {
        match color {
            // exact
            ColorFormat::RGB_U8 => Self::new(ColorType::Rgb8, color),
            ColorFormat::RGB_U16 => Self::new(ColorType::Rgb16, color),
            ColorFormat::RGB_F32 => Self::new(ColorType::Rgb32F, color),
            ColorFormat::GRAYSCALE_U8 => Self::new(ColorType::L8, color),
            ColorFormat::GRAYSCALE_U16 => Self::new(ColorType::L16, color),
            ColorFormat::RGBA_U8 => Self::new(ColorType::Rgba8, color),
            ColorFormat::RGBA_U16 => Self::new(ColorType::Rgba16, color),
            ColorFormat::RGBA_F32 => Self::new(ColorType::Rgba32F, color),
            // widen
            ColorFormat::ALPHA_U8 => Self::new(ColorType::Rgba8, ColorFormat::RGBA_U8),
            ColorFormat::ALPHA_U16 => Self::new(ColorType::Rgba16, ColorFormat::RGBA_U16),
            ColorFormat::ALPHA_F32 => Self::new(ColorType::Rgba32F, ColorFormat::RGBA_F32),
            ColorFormat::GRAYSCALE_F32 => Self::new(ColorType::Rgb32F, ColorFormat::RGB_F32),
        }
    }
}

fn to_image_error(e: dds::DecodingError) -> ImageError {
    match e {
        dds::DecodingError::Io(e) => ImageError::IoError(e),
        dds::DecodingError::MemoryLimitExceeded => {
            ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
        }
        _ => ImageError::Decoding(DecodingError::new(format_hint(), e)),
    }
}

fn format_hint() -> ImageFormatHint {
    ImageFormatHint::Name("DDS".into())
}

//! Decoding and encoding DDS images
//!
//! DDS (DirectDraw Surface) is a container format for storing uncompressed and
//! BCn/DXT (S3TC) compressed images for use in graphics applications.
//!
//! # Related Links
//!
//! * <https://docs.microsoft.com/en-us/windows/win32/direct3ddds/dx-graphics-dds-pguide> - Description of the DDS format.
//! * <https://microsoft.github.io/DirectX-Specs/d3d/archive/D3D11_3_FunctionalSpec.htm> - Direct3D 11.3 Functional Specification

use std::io::{Read, Seek};

use dds::header::ParseOptions;
use dds::{
    Channels, ColorFormat, DataLayout, Decoder, Format, ImageViewMut, Size, TextureArrayKind,
};

use image::error::{
    DecodingError, ImageError, ImageFormatHint, ImageResult, LimitError, LimitErrorKind,
    ParameterError, ParameterErrorKind, UnsupportedError, UnsupportedErrorKind,
};
use image::io::{
    DecodedImageAttributes, DecodedMetadataHint, DecoderPreparedImage, FormatAttributes,
    SequenceControl,
};
use image::{ColorType, ExtendedColorType, ImageDecoder, Limits};

enum DdsDecodeState<R> {
    Init(R),
    Header(Decoder<R>),
    Done,
}

/// DDS decoder.
///
/// This decoder supports decoding DDS files with a single texture, including
/// cube maps. Texture arrays and volumes are not supported.
///
/// It's possible to set the color type the image is decoded as using
/// [`DdsDecoder::set_color_type`].
pub struct DdsDecoder<R> {
    state: DdsDecodeState<R>,
    requested_color: Option<SupportedColor>,
    limits: Limits,
}

impl<R: Read> DdsDecoder<R> {
    /// Create a new decoder that decodes from the stream `r`
    pub fn new(r: R) -> ImageResult<Self> {
        Ok(DdsDecoder {
            state: DdsDecodeState::Init(r),
            requested_color: None,
            limits: Limits::default(),
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
    pub fn set_color_type(&mut self, color: ColorType) -> ImageResult<()> {
        let Some(supported_color) = SupportedColor::from_image_exact(color) else {
            return Err(ImageError::Unsupported(
                UnsupportedError::from_format_and_kind(
                    format_hint(),
                    UnsupportedErrorKind::Color(color.into()),
                ),
            ));
        };
        self.requested_color = Some(supported_color);
        Ok(())
    }

    /// Like ImageDecoder::prepare_image, but returns some additional information
    fn prepare_image_helper(&mut self) -> ImageResult<(Size, SupportedColor)> {
        if matches!(self.state, DdsDecodeState::Init(_)) {
            let mut state = DdsDecodeState::Done;
            std::mem::swap(&mut state, &mut self.state);
            let DdsDecodeState::Init(reader) = state else {
                unreachable!();
            };

            let options = ParseOptions::new_permissive(None);
            let mut decoder =
                Decoder::new_with_options(reader, &options).map_err(to_image_error)?;
            if let Some(max_alloc) = self.limits.max_alloc {
                decoder.options.memory_limit = max_alloc.try_into().unwrap_or(usize::MAX);
            }
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

            self.state = DdsDecodeState::Header(decoder);
        }

        let decoder = match &self.state {
            DdsDecodeState::Init(_) => unreachable!(),
            DdsDecodeState::Header(inner) => inner,
            DdsDecodeState::Done => {
                return Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::NoMoreData,
                )));
            }
        };

        let mut size = decoder.main_size();
        let mut color = decoder.native_color();
        let is_cubemap = decoder.layout().is_cube_map();

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

        let output_color = if let Some(req_color) = self.requested_color {
            req_color
        } else {
            SupportedColor::from_dds_widen(color)
        };
        Ok((size, output_color))
    }
}

impl<R: Read + Seek> ImageDecoder for DdsDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        let mut attributes = FormatAttributes::default();
        attributes.icc = DecodedMetadataHint::None;
        attributes.exif = DecodedMetadataHint::None;
        attributes.xmp = DecodedMetadataHint::None;
        attributes
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let (size, color) = self.prepare_image_helper()?;
        Ok(DecoderPreparedImage::new(
            size.width,
            size.height,
            color.image,
        ))
    }

    fn set_limits(&mut self, limits: image::Limits) -> ImageResult<()> {
        if let Some(max_alloc) = limits.max_alloc {
            if let DdsDecodeState::Header(inner) = &mut self.state {
                inner.options.memory_limit = max_alloc.try_into().unwrap_or(usize::MAX);
            }
        }

        let info = self.prepare_image()?;
        limits.check_dimensions(info.layout.width, info.layout.height)?;
        Ok(())
    }

    #[track_caller]
    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let (size, color) = self.prepare_image_helper()?;

        let mut state = DdsDecodeState::Done;
        std::mem::swap(&mut self.state, &mut state);
        let DdsDecodeState::Header(mut decoder) = state else {
            unreachable!();
        };

        assert_eq!(
            buf.len(),
            color.dds.buffer_size(size).unwrap(),
            "Buffer len does not match for {:?} and {:?}",
            size,
            color.dds
        );

        let image = ImageViewMut::new(buf, size, color.dds).expect("Invalid buffer length");

        if decoder.layout().is_cube_map() {
            decoder.read_cube_map(image).map_err(to_image_error)?;
        } else {
            decoder.read_surface(image).map_err(to_image_error)?;
        }

        let mut attribs = DecodedImageAttributes::default();
        attribs.original_color_type = Some(match decoder.format() {
            Format::R1_UNORM => ExtendedColorType::L1,
            Format::B4G4R4A4_UNORM | Format::A4B4G4R4_UNORM => ExtendedColorType::Rgba4,
            Format::A8_UNORM => ExtendedColorType::A8,
            _ => SupportedColor::from_dds_widen(decoder.native_color())
                .image
                .into(),
        });
        Ok(attribs)
    }

    fn more_images(&self) -> SequenceControl {
        if matches!(self.state, DdsDecodeState::Done) {
            SequenceControl::None
        } else {
            SequenceControl::MaybeMore
        }
    }
}

/// A color type supported by both the `image` and `dds` crates.
#[derive(Clone, Copy)]
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

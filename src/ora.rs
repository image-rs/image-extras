//! Decoding of OpenRaster Images (*.ora)
//!
//! OpenRaster is an a file format used to communicate layered images; the
//! decoder provided herein only extracts and displays the final merged raster
//! image cached by the OpenRaster file, and does not expose the details of
//! layers (which may be either raster or vector graphics) or render the merged
//! image itself.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/OpenRaster> - The OpenRaster format on Wikipedia
//! * <https://www.openraster.org/> - OpenRaster specification

use image::codecs::png::PngDecoder;
use image::error::{DecodingError, ImageFormatHint, UnsupportedError};
use image::metadata::Orientation;
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageError, ImageResult, Limits};
use std::io::{Cursor, Read, Seek};
use std::marker::PhantomData;
use zip::read::ZipArchive;

pub struct OpenRasterDecoder<R>
where
    R: Read + Seek,
{
    mergedimg_decoder: PngDecoder<Cursor<Vec<u8>>>,
    _phantom: PhantomData<R>,
}

fn openraster_format_hint() -> ImageFormatHint {
    ImageFormatHint::Name("OpenRaster".into())
}

/// Adjust the format of the PngDecoder's errors to indicate OpenRaster instead
fn set_ora_image_type(err: ImageError) -> ImageError {
    match err {
        ImageError::Decoding(e) => {
            // DecodingError does not directly expose the underlying type,
            // so nest the error
            ImageError::Decoding(DecodingError::new(openraster_format_hint(), e))
        }
        ImageError::Encoding(_) => {
            // Should not be encoding any files
            unreachable!();
        }
        ImageError::Parameter(e) => ImageError::Parameter(e),
        ImageError::Limits(e) => ImageError::Limits(e),
        ImageError::Unsupported(e) => ImageError::Unsupported(
            UnsupportedError::from_format_and_kind(openraster_format_hint(), e.kind()),
        ),
        ImageError::IoError(e) => ImageError::IoError(e),
    }
}

impl<R> OpenRasterDecoder<R>
where
    R: Read + Seek,
{
    /// Create a new `OpenRasterDecoder` with the provided limits.
    ///
    /// (Limits need to be specified in advance, because determining the
    /// minimum information needed for the ImageDecoder trait (image size and
    /// color type) may require reading through and remembering image-dependent
    /// amount of data.)
    ///
    /// Warning: While decoding limits apply to the header parsing and decoding
    /// of the merged imaged component (a PNG file inside the ZIP archive that
    /// forms an OpenRaster file), memory constraints on the ZIP file decoding
    /// process have not yet been implemented; input ZIP files with very many
    /// entries may require significant amounts of memory to read.
    pub fn with_limits(r: R, mut limits: Limits) -> Result<OpenRasterDecoder<R>, ImageError> {
        let mut archive = ZipArchive::new(r)
            .map_err(|e| ImageError::Decoding(DecodingError::new(openraster_format_hint(), e)))?;

        /* Verify that this _is_ an OpenRaster file, and not some unrelated ZIP archive */
        let mimetype_index = archive.index_for_name("mimetype").ok_or_else(|| {
            ImageError::Decoding(DecodingError::new(
                openraster_format_hint(),
                "OpenRaster images should contain a mimetype subfile",
            ))
        })?;

        let mut mimetype_file = archive
            .by_index(mimetype_index)
            .map_err(|x| ImageError::Decoding(DecodingError::new(openraster_format_hint(), x)))?;

        const EXPECTED_MIMETYPE: &str = "image/openraster";
        let mut tmp = [0u8; EXPECTED_MIMETYPE.len()];

        mimetype_file.read_exact(&mut tmp)?;

        if tmp != EXPECTED_MIMETYPE.as_bytes()
            || mimetype_file.size() != EXPECTED_MIMETYPE.len() as u64
        {
            return Err(ImageError::Decoding(DecodingError::new(
                openraster_format_hint(),
                "Image did not have correct mimetype subentry to be identified as OpenRaster",
            )));
        }

        drop(mimetype_file);

        /* Read the full PNG into a buffer. */
        let mergedimage_index = archive.index_for_name("mergedimage.png").ok_or_else(|| {
            ImageError::Decoding(DecodingError::new(
                openraster_format_hint(),
                "OpenRaster image missing mergedimage.png entry",
            ))
        })?;

        let mut mergedimage = archive
            .by_index(mergedimage_index)
            .map_err(|x| ImageError::Decoding(DecodingError::new(openraster_format_hint(), x)))?;
        let size = mergedimage.size();
        if size > isize::MAX as u64 {
            return Err(ImageError::Limits(image::error::LimitError::from_kind(
                image::error::LimitErrorKind::InsufficientMemory,
            )));
        }

        limits.reserve(size)?;
        let mut buf = Vec::new();
        buf.try_reserve_exact(size as usize)?;

        mergedimage.read_to_end(&mut buf)?;

        let decoder =
            PngDecoder::with_limits(Cursor::new(buf), limits).map_err(set_ora_image_type)?;

        Ok(OpenRasterDecoder {
            mergedimg_decoder: decoder,
            _phantom: PhantomData,
        })
    }
}

impl<R: Read + Seek> ImageDecoder for OpenRasterDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.mergedimg_decoder.dimensions()
    }

    fn color_type(&self) -> ColorType {
        self.mergedimg_decoder.color_type()
    }

    fn original_color_type(&self) -> ExtendedColorType {
        self.mergedimg_decoder.original_color_type()
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        // Warning: this does not account for any ZIP reading overhead
        self.mergedimg_decoder.set_limits(limits)
    }

    fn icc_profile(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.mergedimg_decoder.icc_profile()
    }

    fn exif_metadata(&mut self) -> ImageResult<Option<Vec<u8>>> {
        self.mergedimg_decoder.exif_metadata()
    }

    fn orientation(&mut self) -> ImageResult<Orientation> {
        self.mergedimg_decoder.orientation()
    }

    fn read_image(self, buf: &mut [u8]) -> ImageResult<()> {
        self.mergedimg_decoder.read_image(buf)
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

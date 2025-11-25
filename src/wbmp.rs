//! Encoding and Decoding of WBMP Images
//!
//! WBMP (Wireless BitMaP) Format is an image format used by the WAP protocol.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/Wireless_Application_Protocol_Bitmap_Format> - The WBMP format on Wikipedia
//! * <https://www.wapforum.org/what/technical/SPEC-WAESpec-19990524.pdf> - The WAP Specification

use std::io::{BufRead, Seek, Write};

use image::error::{
    DecodingError, EncodingError, ImageFormatHint, UnsupportedError, UnsupportedErrorKind,
};
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageEncoder, ImageError, ImageResult};

/// Encoder for Wbmp images.
pub struct WbmpEncoder<'a, W> {
    writer: Option<W>,
    inner: Option<wbmp::Encoder<'a, W>>,
    threshold: u8,
}

impl<'a, W: Write> WbmpEncoder<'a, W> {
    pub fn new(writer: W) -> Result<WbmpEncoder<'a, W>, ImageError> {
        let threshold = 127_u8;

        Ok(WbmpEncoder {
            writer: Some(writer),
            inner: None,
            threshold,
        })
    }

    pub fn with_threshold(mut self, threshold: u8) -> WbmpEncoder<'a, W> {
        self.threshold = threshold;
        self
    }
}

impl<'a, W: Write> ImageEncoder for WbmpEncoder<'a, W> {
    fn write_image(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> std::result::Result<(), ImageError> {
        let color = match color_type {
            ExtendedColorType::L8 => wbmp::ColorType::Luma8,
            ExtendedColorType::Rgba8 => wbmp::ColorType::Rgba8,
            _ => {
                return Err(ImageError::Encoding(EncodingError::from_format_hint(
                    ImageFormatHint::Name("Unsupported ColorType".to_string()),
                )));
            }
        };

        if let Some(mut inner) = self.inner {
            inner.encode(buf, width, height, color)
        } else {
            let mut writer = self.writer.take().unwrap();
            let mut encoder = wbmp::Encoder::new(&mut writer);
            encoder.encode(buf, width, height, color)
        }
        .map_err(convert_wbmp_error)?;
        Ok(())
    }
}

/// Decoder for Wbmp images.
pub struct WbmpDecoder<R> {
    dimensions: (u32, u32),
    inner: wbmp::Decoder<R>,
}

impl<R> WbmpDecoder<R>
where
    R: BufRead + Seek,
{
    /// Create a new `WbmpDecoder`.
    pub fn new(r: R) -> Result<WbmpDecoder<R>, ImageError> {
        let inner = wbmp::Decoder::new(r).map_err(convert_wbmp_error)?;
        let dimensions = inner.dimensions();

        Ok(WbmpDecoder { dimensions, inner })
    }
}

fn convert_wbmp_error(err: wbmp::error::WbmpError) -> ImageError {
    use wbmp::error::WbmpError;
    match err {
        WbmpError::IoError(inner) => ImageError::IoError(inner),
        WbmpError::UnsupportedType(inner) => {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormatHint::Name("WBMP".to_string()),
                UnsupportedErrorKind::GenericFeature(format!(
                    "type {} is not supported for wbmp images",
                    inner
                )),
            ))
        }
        WbmpError::UnsupportedHeaders => {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                ImageFormatHint::Name("WBMP".to_string()),
                UnsupportedErrorKind::GenericFeature(
                    "Extension headers are not supported for wbmp images".to_string(),
                ),
            ))
        }
        WbmpError::InvalidImageData => ImageError::Decoding(DecodingError::from_format_hint(
            ImageFormatHint::Name("WBMP".to_string()),
        )),
        WbmpError::UsageError(inner) => ImageError::Decoding(DecodingError::new(
            ImageFormatHint::Name("WBMP".to_string()),
            Box::new(WbmpError::UsageError(inner)),
        )),
    }
}

impl<R: BufRead + Seek> ImageDecoder for WbmpDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.dimensions
    }

    fn color_type(&self) -> ColorType {
        ColorType::L8
    }

    fn original_color_type(&self) -> ExtendedColorType {
        ExtendedColorType::L1
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        self.inner
            .read_image_data(buf)
            .map_err(convert_wbmp_error)?;
        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)?;
        Ok(())
    }
}

//! Encoding and Decoding of WBMP Images
//!
//! WBMP (Wireless BitMaP) Format is an image format used by the WAP protocol.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/Wireless_Application_Protocol_Bitmap_Format> - The WBMP format on Wikipedia
//! * <https://www.wapforum.org/what/technical/SPEC-WAESpec-19990524.pdf> - The WAP Specification

use std::io::{BufRead, Seek, Write};

use image::error::{
    DecodingError, EncodingError, ImageFormatHint, ParameterError, ParameterErrorKind,
    UnsupportedError, UnsupportedErrorKind,
};
use image::io::{
    DecodedImageAttributes, DecodedMetadataHint, DecoderPreparedImage, FormatAttributes,
    SequenceControl,
};
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageEncoder, ImageError, ImageResult};

/// Encoder for Wbmp images.
pub struct WbmpEncoder<W> {
    writer: W,
    threshold: u8,
}

impl<W: Write> WbmpEncoder<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            threshold: 127_u8,
        }
    }

    pub fn with_threshold(mut self, threshold: u8) -> Self {
        self.threshold = threshold;
        self
    }
}

impl<W: Write> ImageEncoder for WbmpEncoder<W> {
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
                return Err(ImageError::Encoding(EncodingError::new(
                    format_hint(),
                    "Unsupported ColorType".to_string(),
                )));
            }
        };

        wbmp::Encoder::new(&mut self.writer)
            .with_threshold(self.threshold)
            .encode(buf, width, height, color)
            .map_err(convert_wbmp_error)
    }
}

enum WbmpDecodeState<R> {
    Init(R),
    Header(wbmp::Decoder<R>),
    Done,
}

/// Decoder for Wbmp images.
pub struct WbmpDecoder<R> {
    state: WbmpDecodeState<R>,
}

impl<R> WbmpDecoder<R>
where
    R: BufRead + Seek,
{
    /// Create a new `WbmpDecoder`.
    pub fn new(r: R) -> Result<WbmpDecoder<R>, ImageError> {
        Ok(WbmpDecoder {
            state: WbmpDecodeState::Init(r),
        })
    }
}

impl<R: BufRead + Seek> ImageDecoder for WbmpDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        let mut attributes = FormatAttributes::default();
        attributes.icc = DecodedMetadataHint::None;
        attributes.exif = DecodedMetadataHint::None;
        attributes.xmp = DecodedMetadataHint::None;
        attributes
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let dimensions = match &self.state {
            WbmpDecodeState::Init(_) => {
                let mut state = WbmpDecodeState::Done;
                std::mem::swap(&mut state, &mut self.state);
                let WbmpDecodeState::Init(reader) = state else {
                    unreachable!();
                };

                let inner = wbmp::Decoder::new(reader).map_err(convert_wbmp_error)?;
                let dimensions = inner.dimensions();
                self.state = WbmpDecodeState::Header(inner);
                dimensions
            }
            WbmpDecodeState::Header(decoder) => decoder.dimensions(),

            WbmpDecodeState::Done => {
                return Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::NoMoreData,
                )));
            }
        };

        Ok(DecoderPreparedImage::new(
            dimensions.0,
            dimensions.1,
            ColorType::L8,
        ))
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let info = self.prepare_image()?;
        assert_eq!(buf.len() as u64, info.total_bytes(), "Invalid buffer size");

        let WbmpDecodeState::Header(decoder) = &mut self.state else {
            unreachable!();
        };
        decoder.read_image_data(buf).map_err(convert_wbmp_error)?;
        self.state = WbmpDecodeState::Done;

        let mut attribs = DecodedImageAttributes::default();
        attribs.original_color_type = Some(ExtendedColorType::L1);
        Ok(attribs)
    }

    fn more_images(&self) -> SequenceControl {
        if matches!(self.state, WbmpDecodeState::Done) {
            SequenceControl::None
        } else {
            SequenceControl::MaybeMore
        }
    }
}

fn convert_wbmp_error(err: wbmp::error::WbmpError) -> ImageError {
    use wbmp::error::WbmpError;
    match err {
        WbmpError::IoError(inner) => ImageError::IoError(inner),
        WbmpError::UnsupportedType(inner) => {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                format_hint(),
                UnsupportedErrorKind::GenericFeature(format!(
                    "type {inner} is not supported for wbmp images"
                )),
            ))
        }
        WbmpError::UnsupportedHeaders => {
            ImageError::Unsupported(UnsupportedError::from_format_and_kind(
                format_hint(),
                UnsupportedErrorKind::GenericFeature(
                    "Extension headers are not supported for wbmp images".to_string(),
                ),
            ))
        }
        WbmpError::InvalidImageData => ImageError::Encoding(EncodingError::new(format_hint(), err)),
        WbmpError::UsageError(_) => ImageError::Decoding(DecodingError::new(format_hint(), err)),
    }
}

fn format_hint() -> ImageFormatHint {
    ImageFormatHint::Name("WBMP".to_string())
}

//! Decoding of PCX Images
//!
//! PCX (PiCture eXchange) Format is an obsolete image format from the 1980s.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/PCX> - The PCX format on Wikipedia

use std::io::{self, BufRead, Read, Seek};
use std::iter;

use image::error::{ParameterError, ParameterErrorKind};
use image::io::{
    DecodedImageAttributes, DecodedMetadataHint, DecoderPreparedImage, FormatAttributes,
    SequenceControl,
};
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageError, ImageResult};

enum PcxDecodeState<R>
where
    R: Read,
{
    Init(R),
    Header(pcx::Reader<R>),
    Done,
}

/// Decoder for PCX images.
pub struct PCXDecoder<R>
where
    R: Read,
{
    state: PcxDecodeState<R>,
}

impl<R> PCXDecoder<R>
where
    R: BufRead + Seek,
{
    /// Create a new `PCXDecoder`.
    pub fn new(r: R) -> Result<PCXDecoder<R>, ImageError> {
        Ok(PCXDecoder {
            state: PcxDecodeState::Init(r),
        })
    }
}

fn convert_pcx_decode_error(err: io::Error) -> ImageError {
    ImageError::IoError(err)
}

impl<R: BufRead + Seek> ImageDecoder for PCXDecoder<R> {
    fn format_attributes(&self) -> FormatAttributes {
        let mut attributes = FormatAttributes::default();
        attributes.icc = DecodedMetadataHint::None;
        attributes.exif = DecodedMetadataHint::None;
        attributes.xmp = DecodedMetadataHint::None;
        attributes
    }

    fn prepare_image(&mut self) -> ImageResult<DecoderPreparedImage> {
        let dimensions = match &self.state {
            PcxDecodeState::Init(_) => {
                let mut state = PcxDecodeState::Done;
                std::mem::swap(&mut state, &mut self.state);
                let PcxDecodeState::Init(reader) = state else {
                    unreachable!();
                };
                let inner = pcx::Reader::new(reader).map_err(convert_pcx_decode_error)?;
                let dimensions = inner.dimensions();
                self.state = PcxDecodeState::Header(inner);
                dimensions
            }
            PcxDecodeState::Header(inner) => inner.dimensions(),

            PcxDecodeState::Done => {
                return Err(ImageError::Parameter(ParameterError::from_kind(
                    ParameterErrorKind::NoMoreData,
                )));
            }
        };

        Ok(DecoderPreparedImage::new(
            u32::from(dimensions.0),
            u32::from(dimensions.1),
            ColorType::Rgb8,
        ))
    }

    fn read_image(&mut self, buf: &mut [u8]) -> ImageResult<DecodedImageAttributes> {
        let info = self.prepare_image()?;
        assert_eq!(u64::try_from(buf.len()), Ok(info.total_bytes()));

        let mut state = PcxDecodeState::Done;
        std::mem::swap(&mut state, &mut self.state);
        let PcxDecodeState::Header(mut inner) = state else {
            unreachable!();
        };

        let orig_color_type = if inner.is_paletted() {
            ExtendedColorType::Unknown(inner.header.bit_depth)
        } else {
            match (inner.header.number_of_color_planes, inner.header.bit_depth) {
                (1, 1) => ExtendedColorType::L1,
                (1, 2) => ExtendedColorType::L2,
                (1, 4) => ExtendedColorType::L4,
                (1, 8) => ExtendedColorType::L8,
                (3, 1) => ExtendedColorType::Rgb1,
                (3, 2) => ExtendedColorType::Rgb2,
                (3, 4) => ExtendedColorType::Rgb4,
                (3, 8) => ExtendedColorType::Rgb8,
                (4, 1) => ExtendedColorType::Rgba1,
                (4, 2) => ExtendedColorType::Rgba2,
                (4, 4) => ExtendedColorType::Rgba4,
                (4, 8) => ExtendedColorType::Rgba8,
                (_, _) => unreachable!(),
            }
        };

        let height = inner.height() as usize;
        let width = inner.width() as usize;

        match inner.palette_length() {
            // No palette to interpret, so we can just write directly to buf
            None => {
                for i in 0..height {
                    let offset = i * 3 * width;
                    inner
                        .next_row_rgb(&mut buf[offset..offset + (width * 3)])
                        .map_err(convert_pcx_decode_error)?;
                }
            }

            // We need to convert from the palette colours to RGB values inline,
            // but the pcx crate can't give us the palette first. Work around it
            // by taking the paletted image into a buffer, then converting it to
            // RGB8 after.
            Some(palette_length) => {
                let mut pal_buf: Vec<u8> = iter::repeat_n(0, height * width).collect();

                for i in 0..height {
                    let offset = i * width;
                    inner
                        .next_row_paletted(&mut pal_buf[offset..offset + width])
                        .map_err(convert_pcx_decode_error)?;
                }

                let mut palette: Vec<u8> =
                    std::iter::repeat_n(0, 3 * palette_length as usize).collect();
                inner
                    .read_palette(&mut palette[..])
                    .map_err(convert_pcx_decode_error)?;

                for i in 0..height {
                    for j in 0..width {
                        let pixel = pal_buf[i * width + j] as usize;
                        let offset = i * width * 3 + j * 3;

                        buf[offset] = palette[pixel * 3];
                        buf[offset + 1] = palette[pixel * 3 + 1];
                        buf[offset + 2] = palette[pixel * 3 + 2];
                    }
                }
            }
        }

        let mut attribs = DecodedImageAttributes::default();
        attribs.original_color_type = Some(orig_color_type);
        Ok(attribs)
    }

    fn more_images(&self) -> SequenceControl {
        if matches!(self.state, PcxDecodeState::Done) {
            SequenceControl::None
        } else {
            SequenceControl::MaybeMore
        }
    }
}

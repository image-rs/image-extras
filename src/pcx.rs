//! Decoding of PCX Images
//!
//! PCX (PiCture eXchange) Format is an obsolete image format from the 1980s.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/PCX> - The PCX format on Wikipedia

use std::io::{self, BufRead, Read, Seek};
use std::iter;

use image::{ColorType, ExtendedColorType, ImageDecoder, ImageError, ImageResult};

/// Decoder for PCX images.
pub struct PCXDecoder<R>
where
    R: Read,
{
    dimensions: (u32, u32),
    inner: pcx::Reader<R>,
}

impl<R> PCXDecoder<R>
where
    R: BufRead + Seek,
{
    /// Create a new `PCXDecoder`.
    pub fn new(r: R) -> Result<PCXDecoder<R>, ImageError> {
        let inner = pcx::Reader::new(r).map_err(convert_pcx_decode_error)?;
        let dimensions = (u32::from(inner.width()), u32::from(inner.height()));

        Ok(PCXDecoder { dimensions, inner })
    }
}

fn convert_pcx_decode_error(err: io::Error) -> ImageError {
    ImageError::IoError(err)
}

impl<R: BufRead + Seek> ImageDecoder for PCXDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        self.dimensions
    }

    fn color_type(&self) -> ColorType {
        ColorType::Rgb8
    }

    fn original_color_type(&self) -> ExtendedColorType {
        if self.inner.is_paletted() {
            return ExtendedColorType::Unknown(self.inner.header.bit_depth);
        }

        match (
            self.inner.header.number_of_color_planes,
            self.inner.header.bit_depth,
        ) {
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
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let height = self.inner.height() as usize;
        let width = self.inner.width() as usize;

        match self.inner.palette_length() {
            // No palette to interpret, so we can just write directly to buf
            None => {
                for i in 0..height {
                    let offset = i * 3 * width;
                    self.inner
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
                    self.inner
                        .next_row_paletted(&mut pal_buf[offset..offset + width])
                        .map_err(convert_pcx_decode_error)?;
                }

                let mut palette: Vec<u8> =
                    std::iter::repeat_n(0, 3 * palette_length as usize).collect();
                self.inner
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

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

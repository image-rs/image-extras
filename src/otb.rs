//! Decoding of OTB Images
//!
//! OTB (Over The air Bitmap) Format is an image format from Nokia's Smart Messaging specification.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/OTA_bitmap> - OTA bitmap on Wikipedia
//! * <https://web.archive.org/web/20151028024229/https://www.csoft.co.uk/documents/sms3_0_0.pdf> - Specification

use std::fmt::{self, Display};
use std::io::{BufRead, Seek, Write};

use image::error::{DecodingError, EncodingError, ImageFormatHint, LimitError, LimitErrorKind};
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageEncoder, ImageError, ImageResult};

/// All errors that can occur when attempting to encode an image to OTB format
#[derive(Debug, Clone)]
enum EncoderError {
    /// Specified image does not fit into OTB width / height constraints
    ImageTooLarge,

    /// ColorType of image is not supported
    UnsupportedColorType,
}

impl Display for EncoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EncoderError::ImageTooLarge => f.write_fmt(format_args!(
                "Specified image is too large for the OTB format (Max 255x255)"
            )),
            EncoderError::UnsupportedColorType => f.write_fmt(format_args!(
                "Color type of specified image is not supported"
            )),
        }
    }
}
impl std::error::Error for EncoderError {}

impl From<EncoderError> for ImageError {
    fn from(e: EncoderError) -> ImageError {
        match e {
            EncoderError::ImageTooLarge => {
                ImageError::Limits(LimitError::from_kind(LimitErrorKind::DimensionError))
            }
            _ => ImageError::Encoding(EncodingError::new(ImageFormatHint::Name("otb".into()), e)),
        }
    }
}

/// Encoder for Otb images.
pub struct OtbEncoder<W> {
    writer: W,
    threshold: u8,
}

impl<W: Write> OtbEncoder<W> {
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

impl<W: Write> ImageEncoder for OtbEncoder<W> {
    fn write_image(
        mut self,
        buf: &[u8],
        width: u32,
        height: u32,
        color_type: ExtendedColorType,
    ) -> std::result::Result<(), ImageError> {
        if width > 0xFF || height > 0xFF {
            return Err(EncoderError::ImageTooLarge.into());
        }
        if color_type != ExtendedColorType::L8 {
            return Err(EncoderError::UnsupportedColorType.into());
        }

        // Write Headers
        let _ = self
            .writer
            .write(&[0x00, width as u8, height as u8, 0x01])?;

        // Write the encoded image
        let mut current_byte = 0_u8;
        let mut bit = 0;
        for buf_idx in 0..(width * height) {
            if buf[buf_idx as usize] < self.threshold {
                current_byte |= 1 << (7 - bit);
            }
            bit += 1;
            if bit == 8 {
                self.writer.write_all(&[current_byte])?;
                current_byte = 0_u8;
                bit = 0;
            };
        }
        if bit != 0 {
            self.writer.write_all(&[current_byte])?;
        }

        Ok(())
    }
}

/// All errors that can occur when attempting to parse an OTB image
#[derive(Debug, Clone)]
enum DecoderError {
    /// Info field in OTB image headers is unsupported
    UnsupportedInfoField(u8),
    /// Width in OTB image headers is zero
    WidthZero,
    /// Height in OTB image headers is zero
    HeightZero,
    /// Specified color depth is not supported
    UnsupportedColorDepth(u8),
}

impl Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnsupportedInfoField(info) => {
                f.write_fmt(format_args!("Unsupported value in info field {info:08b}"))
            }
            Self::WidthZero => f.write_fmt(format_args!("Width cannot be zero")),
            Self::HeightZero => f.write_fmt(format_args!("Height cannot be zero")),
            Self::UnsupportedColorDepth(depth) => f.write_fmt(format_args!(
                "Unsupported color depth value in headers {depth}"
            )),
        }
    }
}
impl std::error::Error for DecoderError {}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormatHint::Name("otb".into()), e))
    }
}

/// Decoder for Otb images.
pub struct OtbDecoder<R> {
    reader: R,
    dimensions: (u32, u32),
}

impl<R> OtbDecoder<R>
where
    R: BufRead + Seek,
{
    /// Create a new `OtbDecoder`.
    pub fn new(reader: R) -> Result<OtbDecoder<R>, ImageError> {
        let mut decoder = Self::new_decoder(reader);
        decoder.read_metadata()?;
        Ok(decoder)
    }

    fn new_decoder(reader: R) -> OtbDecoder<R> {
        Self {
            reader,
            dimensions: (0, 0),
        }
    }

    fn read_metadata(&mut self) -> Result<(), ImageError> {
        let mut header_buf = [0_u8; 4];
        self.reader.read_exact(&mut header_buf)?;

        let [info_field, width, height, depth] = header_buf;

        // InfoField - 00 for single byte width/height values
        if info_field != 0 {
            return Err(DecoderError::UnsupportedInfoField(info_field).into());
        }

        // Width
        if width == 0 {
            return Err(DecoderError::WidthZero.into());
        }

        // Height
        if height == 0 {
            return Err(DecoderError::HeightZero.into());
        }

        // Depth
        if depth != 1 {
            return Err(DecoderError::UnsupportedColorDepth(depth).into());
        }

        self.dimensions = (width as u32, height as u32);

        Ok(())
    }
}

impl<R: BufRead + Seek> ImageDecoder for OtbDecoder<R> {
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
        let (width, height) = (self.dimensions.0 as usize, self.dimensions.1 as usize);

        assert_eq!(buf.len(), width * height, "Invalid buffer length");

        // Read entire image data into a buffer
        let mut byte_buf = vec![0_u8; (width * height).div_ceil(8)].into_boxed_slice();
        self.reader.read_exact(&mut byte_buf)?;

        // Set a byte in buf for every bit in the image data
        for (i, &byte) in byte_buf.iter().enumerate() {
            for bit in 0..8 {
                let buf_idx = 8 * i + bit;
                if buf_idx >= buf.len() {
                    break;
                }

                buf[buf_idx] = if (byte >> (7 - bit)) & 1 == 0 {
                    0xFF
                } else {
                    0x00
                };
            }
        }
        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

mod test {
    #[test]
    fn test_decode_image() {
        use image::ImageDecoder;
        use std::io::Cursor;
        let otb_data = vec![
            // Headers
            0x00, 0x48, 0x1C, 0x01, // End Headers
            // Image Data
            0x7F, 0xFF, 0xEF, 0xFF, 0xEF, 0xFF, 0xFB, 0xFF, 0xFE, 0x40, 0x3F, 0xE8, 0x38, 0x2F,
            0xFF, 0xFB, 0xFF, 0xFE, 0x48, 0x3F, 0xA8, 0x38, 0x2F, 0x9F, 0xFB, 0xFF, 0xFE, 0x4C,
            0xFF, 0xA9, 0xFF, 0x2F, 0x8F, 0xFA, 0xDA, 0xDA, 0x4E, 0xFF, 0x29, 0x01, 0x2F, 0x80,
            0xFA, 0x52, 0x52, 0x5E, 0x7F, 0x69, 0x31, 0x2F, 0xBF, 0x7B, 0x07, 0x06, 0x4F, 0xFF,
            0x69, 0x79, 0x2F, 0xBE, 0xFB, 0x77, 0x76, 0x47, 0xFF, 0x69, 0x79, 0x2F, 0xBE, 0x7B,
            0x07, 0x06, 0x47, 0xFE, 0xEF, 0x7D, 0xEF, 0xBE, 0x7B, 0xFF, 0xFE, 0x47, 0xFC, 0xEF,
            0x7D, 0xE7, 0xBC, 0xF1, 0xFF, 0xFC, 0x40, 0xF0, 0xEF, 0x7D, 0xE7, 0x7C, 0xF1, 0xED,
            0xBC, 0x21, 0xE7, 0xC9, 0x79, 0x27, 0x98, 0xF1, 0xE5, 0x3C, 0x21, 0xE7, 0xC9, 0x39,
            0x27, 0xC8, 0xF1, 0xF0, 0x7C, 0x16, 0x6F, 0x89, 0x39, 0x23, 0xE6, 0xE0, 0xF7, 0x78,
            0x15, 0x2F, 0x88, 0x82, 0x23, 0xF3, 0xE0, 0xF0, 0x78, 0x08, 0x3F, 0x04, 0x44, 0x43,
            0xD7, 0xE0, 0xFF, 0xF8, 0x04, 0x3E, 0x02, 0x28, 0x81, 0xEF, 0xC0, 0x7F, 0xF0, 0x02,
            0x3C, 0x01, 0x39, 0x00, 0xFF, 0x80, 0x3F, 0xE0, 0x01, 0x38, 0x00, 0xBA, 0x00, 0x7F,
            0x00, 0x1F, 0xC0, 0x00, 0xF0, 0x00, 0x7C, 0x00, 0x3E, 0x00, 0x0F, 0x80, 0xFF, 0xC0,
            0x00, 0x38, 0x00, 0x1C, 0x00, 0x07, 0xFF, 0x55, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xAA, 0x2A, 0xF3, 0x87, 0x87, 0x3F, 0x1E, 0x67, 0x0F, 0x54, 0x15, 0xF3, 0x93,
            0x9F, 0x3E, 0x4E, 0x27, 0x27, 0xA8, 0x2A, 0xF3, 0x87, 0x8F, 0x3E, 0x4E, 0x07, 0x27,
            0x54, 0x55, 0xF3, 0x93, 0x9F, 0x3E, 0x0E, 0x47, 0x27, 0xAA, 0xFF, 0xF3, 0x9B, 0x87,
            0x0E, 0x4E, 0x67, 0x0F, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            0x00, // End Image Data
        ];
        let decoder = crate::otb::OtbDecoder::new(Cursor::new(otb_data)).unwrap();
        let (width, height) = decoder.dimensions();
        assert!(width == 0x48);
        assert!(height == 0x1C);
        let mut img_bytes = vec![0; 2016];
        decoder.read_image(&mut img_bytes).unwrap();
    }

    #[test]
    fn test_decoder_irregular_width() {
        use image::ImageDecoder;
        use std::io::Cursor;
        let expected_data = [
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // row0
            0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, // row1
            0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, // row2
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row3
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row4
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row5
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row6
            0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, // row7
            0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, // row8
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // row9
        ];
        #[allow(clippy::unusual_byte_groupings)]
        let image_data: [u8; 17] = [
            0,
            10,
            10,
            1, // headers
            0b_00000000,
            0b00_000011,
            0b0000_0001,
            0b001000_00,
            0b10000100_,
            0b_01000000,
            0b10_010000,
            0b0010_0010,
            0b000100_00,
            0b01001000_,
            0b_00001100,
            0b00_000000,
            0b0000_0000,
        ];
        let decoder = crate::otb::OtbDecoder::new(Cursor::new(image_data)).unwrap();
        let (width, height) = decoder.dimensions();
        assert!(width == 10);
        assert!(height == 10);
        let mut img_bytes = vec![0; 100];
        decoder.read_image(&mut img_bytes).unwrap();
        img_bytes.iter().enumerate().for_each(|(i, byte)| {
            assert_eq!(*byte, expected_data[i]);
        });
    }

    #[test]
    fn test_decoder() {
        use image::ImageDecoder;
        use std::io::Cursor;
        let expected_data = [
            0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, // row1
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row2
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row3
            0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, // row4
            0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, // row5
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row6
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row7
            0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, // row8
        ];
        let image_data: [u8; 12] = [
            0, 8, 8, 1,          // headers
            0b00011000, // row1
            0b00100100, // row2
            0b01000010, // row3
            0b10000001, // row4
            0b10000001, // row5
            0b01000010, // row6
            0b00100100, // row7
            0b00011000, // row8
        ];
        let decoder = crate::otb::OtbDecoder::new(Cursor::new(image_data)).unwrap();
        let (width, height) = decoder.dimensions();
        assert!(width == 8);
        assert!(height == 8);
        let mut img_bytes = vec![0; 64];
        decoder.read_image(&mut img_bytes).unwrap();
        img_bytes.iter().enumerate().for_each(|(i, byte)| {
            assert_eq!(*byte, expected_data[i]);
        });
    }

    #[test]
    fn test_encoder() {
        use image::ImageEncoder;
        let img_data = [
            0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, // row1
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row2
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row3
            0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, // row4
            0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, // row5
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row6
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row7
            0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, // row8
        ];
        let expected_data: [u8; 12] = [
            0, 8, 8, 1,          // headers
            0b00011000, // row1
            0b00100100, // row2
            0b01000010, // row3
            0b10000001, // row4
            0b10000001, // row5
            0b01000010, // row6
            0b00100100, // row7
            0b00011000, // row8
        ];
        let mut encoded_data = Vec::<u8>::with_capacity(expected_data.len());
        let encoder = crate::otb::OtbEncoder::new(&mut encoded_data);
        encoder
            .write_image(&img_data, 8, 8, image::ExtendedColorType::L8)
            .unwrap();
        encoded_data.iter().enumerate().for_each(|(i, byte)| {
            assert_eq!(*byte, expected_data[i]);
        });
    }

    #[test]
    fn test_encoder_irregular_width() {
        use image::ImageEncoder;
        let img_data = [
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // row0
            0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, // row1
            0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, // row2
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row3
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row4
            0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, // row5
            0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, // row6
            0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0x00, 0xFF, 0xFF, 0xFF, // row7
            0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, // row8
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // row9
        ];
        #[allow(clippy::unusual_byte_groupings)]
        let expected_data: [u8; 17] = [
            0,
            10,
            10,
            1, // headers
            0b_00000000,
            0b00_000011,
            0b0000_0001,
            0b001000_00,
            0b10000100_,
            0b_01000000,
            0b10_010000,
            0b0010_0010,
            0b000100_00,
            0b01001000_,
            0b_00001100,
            0b00_000000,
            0b0000_0000,
        ];
        let mut encoded_data = Vec::<u8>::with_capacity(expected_data.len());
        let encoder = crate::otb::OtbEncoder::new(&mut encoded_data);
        encoder
            .write_image(&img_data, 10, 10, image::ExtendedColorType::L8)
            .unwrap();
        encoded_data.iter().enumerate().for_each(|(i, byte)| {
            assert_eq!(*byte, expected_data[i]);
        });
    }
}

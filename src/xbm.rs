//! Decoding of X BitMap (.xbm) Images
//!
//! XBM (X BitMap) Format is a plain text image format, sometimes used to store
//! cursor and icon data. XBM images can be valid C code (although a noticeable
//! fraction of historical images includes a name field which is not a valid C
//! identifier, and need not even be either of pure-ASCII or UTF-8).
//!
//! # Related Links
//! * <https://www.x.org/releases/X11R7.7/doc/libX11/libX11/libX11.html#Manipulating_Bitmaps> - The XBM format specification
//! * <https://en.wikipedia.org/wiki/X_BitMap> - The XBM format on wikipedia

use std::fmt;
use std::io::{BufRead, Bytes};

use image::error::{DecodingError, ImageFormatHint, ParameterError, ParameterErrorKind};
use image::{ColorType, ExtendedColorType, ImageDecoder, ImageError, ImageResult};

/// Location of a byte in the input stream.
///
/// Includes byte offset (for format debugging with hex editor) and
/// line:column offset (for format debugging with text editor)
#[derive(Clone, Copy, Debug)]
struct TextLocation {
    byte: u64,
    line: u64,
    column: u64,
}

/// A peekable reader which tracks location information
struct TextReader<R> {
    inner: R,

    current: Option<u8>,

    location: TextLocation,
}

impl<R> TextReader<R>
where
    R: Iterator<Item = u8>,
{
    /// Initialize a TextReader
    fn new(mut r: R) -> TextReader<R> {
        let current = r.next();
        TextReader {
            inner: r,
            current,
            location: TextLocation {
                byte: 0,
                line: 1,
                column: 0,
            },
        }
    }

    /// Consume the next byte. On EOF, will return None
    fn next(&mut self) -> Option<u8> {
        self.current?;

        let mut current = self.inner.next();
        std::mem::swap(&mut self.current, &mut current);

        self.location.byte += 1;
        self.location.column += 1;
        if let Some(b'\n') = current {
            self.location.line += 1;
            self.location.column = 0;
        }
        current
    }
    /// Peek at the next byte. On EOF, will return None
    fn peek(&self) -> Option<u8> {
        self.current
    }
    /// The location of the last byte returned by [Self::next]
    fn loc(&self) -> TextLocation {
        self.location
    }
}

/// Properties of an XBM image (excluding the rarely useful `name` field.)
struct XbmHeaderData {
    width: u32,
    height: u32,
    hotspot: Option<(i32, i32)>,
}

/// XBM stream decoder (works in no_std, has the natural streaming API for the uncompressed text structure of XBM)
///
/// To properly validate the image trailer, invoke `next_byte()` again after reading the last byte of content; if
/// the trailer is valid it should return Ok(None).
struct XbmStreamDecoder<R> {
    r: TextReader<R>,
    current_position: u64,
    // Note: technically this includes header metadata that isn't _needed_ when parsing
    header: XbmHeaderData,
}

/// Helper struct to project BufRead down to Iterator<Item=u8>. Costs of this simple
/// lifetime-free abstraction include that the struct requires space to store the
/// error value, and that code using this must eventually check the error field.
struct IoAdapter<R> {
    reader: Bytes<R>,
    error: Option<std::io::Error>,
}

impl<R> Iterator for IoAdapter<R>
where
    R: BufRead,
{
    type Item = u8;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.error.is_some() {
            return None;
        }
        match self.reader.next() {
            None => None,
            Some(Ok(v)) => Some(v),
            Some(Err(e)) => {
                self.error = Some(e);
                None
            }
        }
    }
}

/// XBM decoder (usable wrapper of XbmStreamDecoder that handles IO errors)
pub struct XbmDecoder<R> {
    base: XbmStreamDecoder<IoAdapter<R>>,
}

/// Part of the XBM file in which a parse error occurs
#[derive(Debug, Clone, Copy)]
enum XbmPart {
    Width,
    Height,
    HotspotX,
    HotspotY,
    Array,
    Data,
    ArrayEnd,
    Trailing,
}

/// Error that can occur while parsing an XBM file
#[derive(Debug)]
enum XbmDecodeError {
    Parse(XbmPart, TextLocation),
    DecodeInteger(XbmPart),
    ZeroWidth,
    ZeroHeight,
}

impl fmt::Display for TextLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "byte={},line={}:col={}",
            self.byte, self.line, self.column
        ))
    }
}

impl fmt::Display for XbmPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            XbmPart::Width => f.write_str("#define for image width"),
            XbmPart::Height => f.write_str("#define for image height"),
            XbmPart::HotspotX => f.write_str("#define for hotspot x coordinate"),
            XbmPart::HotspotY => f.write_str("#define for hotspot y coordinate"),
            XbmPart::Array => f.write_str("array definition"),
            XbmPart::Data => f.write_str("array content"),
            XbmPart::ArrayEnd => f.write_str("array end"),
            XbmPart::Trailing => f.write_str("end of file"),
        }
    }
}

impl fmt::Display for XbmDecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            XbmDecodeError::Parse(part, loc) => f.write_fmt(format_args!(
                "Failed to parse {}, unexpected character or eof at {}",
                part, loc
            )),
            XbmDecodeError::DecodeInteger(part) => {
                f.write_fmt(format_args!("Failed to parse integer for {}", part))
            }
            XbmDecodeError::ZeroWidth => f.write_str("Invalid image width: should not be zero"),
            XbmDecodeError::ZeroHeight => f.write_str("Invalid image height: should not be zero"),
        }
    }
}

impl std::error::Error for XbmDecodeError {}

impl From<XbmDecodeError> for ImageError {
    fn from(e: XbmDecodeError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormatHint::Name("XBM".into()), e))
    }
}

/// Helper trait for the pattern in which, after calling a function returning a Result,
/// one wishes to use an error from a different source.
trait XbmDecoderIoInjectionExt {
    type Value;
    fn apply_after(self, err: &mut Option<std::io::Error>) -> Result<Self::Value, ImageError>;
}

impl<X> XbmDecoderIoInjectionExt for Result<X, XbmDecodeError> {
    type Value = X;
    fn apply_after(self, err: &mut Option<std::io::Error>) -> Result<Self::Value, ImageError> {
        if let Some(err) = err.take() {
            return Err(ImageError::IoError(err));
        }
        match self {
            Self::Ok(x) => Ok(x),
            Self::Err(e) => Err(ImageError::Decoding(DecodingError::new(
                ImageFormatHint::Name("XBM".into()),
                e,
            ))),
        }
    }
}

/// A limit on the length of a #define symbol (containing `name` + '_width') in an image.
/// Names are typically valid C identifiers, and a 255 char limit is common,
/// so XBM files exceeding this are unlikely to work anyway.
const MAX_IDENTIFIER_LENGTH: usize = 256;

/// Read precisely the string `s` from `r`, or error.
fn read_fixed_string<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    s: &[u8],
    part: XbmPart,
) -> Result<(), XbmDecodeError> {
    for c in s {
        if let Some(b) = r.next() {
            if b != *c {
                return Err(XbmDecodeError::Parse(part, r.loc()));
            }
        } else {
            return Err(XbmDecodeError::Parse(part, r.loc()));
        };
    }
    Ok(())
}
// Read a single byte
fn read_byte<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    part: XbmPart,
) -> Result<u8, XbmDecodeError> {
    match r.next() {
        None => Err(XbmDecodeError::Parse(part, r.loc())),
        Some(b) => Ok(b),
    }
}

/// Read a mixture of ' ' and '\t'. At least one character must be read.
// Other whitespace characters are not permitted.
fn read_whitespace_gap<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    part: XbmPart,
) -> Result<(), XbmDecodeError> {
    let b = read_byte(r, part)?;
    if !(b == b' ' || b == b'\t') {
        return Err(XbmDecodeError::Parse(part, r.loc()));
    }
    while let Some(b) = r.peek() {
        if b == b' ' || b == b'\t' {
            r.next();
            continue;
        } else {
            return Ok(());
        }
    }
    Ok(())
}
/// Read a mixture of ' ', '\t', and '\n'. Other whitespace characters are not permitted.
fn read_optional_whitespace<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
) -> Result<(), XbmDecodeError> {
    while let Some(b) = r.peek() {
        if b == b' ' || b == b'\t' || b == b'\n' {
            r.next();
            continue;
        } else {
            break;
        }
    }
    Ok(())
}
/// Read a mixture of ' ' and '\t', until reading '\n'.
fn read_to_newline<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    part: XbmPart,
) -> Result<(), XbmDecodeError> {
    while let Some(b) = r.peek() {
        if b == b' ' || b == b'\t' {
            r.next();
            continue;
        } else {
            break;
        }
    }
    if read_byte(r, part)? != b'\n' {
        Err(XbmDecodeError::Parse(part, r.loc()))
    } else {
        Ok(())
    }
}
/// Read token into the buffer until the buffer size is exceeded, or ' ' or '\t' or '\n' is found
/// Returns the length of the data read.
fn read_until_whitespace<'a, R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    buf: &'a mut [u8],
    part: XbmPart,
) -> Result<&'a [u8], XbmDecodeError> {
    let mut len = 0;
    while let Some(b) = r.peek() {
        if b == b' ' || b == b'\t' || b == b'\n' {
            return Ok(&buf[..len]);
        } else {
            if len >= buf.len() {
                // identifier is too long
                return Err(XbmDecodeError::Parse(part, r.loc()));
            }
            buf[len] = b;
            len += 1;
            r.next();
        }
    }
    Ok(&buf[..len])
}

/// Read a single hex digit, either upper or lower case
fn read_hex_digit<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    part: XbmPart,
) -> Result<u8, XbmDecodeError> {
    let b = read_byte(r, part)?;
    match b {
        b'0'..=b'9' => Ok(b - b'0'),
        b'A'..=b'F' => Ok(b - b'A' + 10),
        b'a'..=b'f' => Ok(b - b'a' + 10),
        _ => Err(XbmDecodeError::Parse(part, r.loc())),
    }
}

/// Read a hex-encoded byte (e.g.: 0xA1)
fn read_hex_byte<R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    part: XbmPart,
) -> Result<u8, XbmDecodeError> {
    if read_byte(r, part)? != b'0' {
        return Err(XbmDecodeError::Parse(part, r.loc()));
    }
    let x = read_byte(r, part)?;
    if !(x == b'x' || x == b'X') {
        return Err(XbmDecodeError::Parse(part, r.loc()));
    }
    let mut v = read_hex_digit(r, part)? << 4;
    v += read_hex_digit(r, part)?;
    Ok(v)
}

/// Parse string into signed integer, rejecting leading + and leading zeros
/// (i32::from_str_radix accepts '014' as 14, but in C is it octal and has value 12)
fn parse_i32(data: &[u8]) -> Option<i32> {
    if data.starts_with(b"-") {
        (-(parse_u32(&data[1..])? as i64)).try_into().ok()
    } else {
        parse_u32(data)?.try_into().ok()
    }
}

/// Parse string into unsigned integer, rejecting leading + and leading zeros
/// (u32::from_str_radix accepts '014' as 14, but in C is it octal and has value 12)
fn parse_u32(data: &[u8]) -> Option<u32> {
    let Some(c1) = data.first() else {
        // Reject empty string
        return None;
    };
    if *c1 == b'0' && data.len() > 1 {
        // Nonzero integers may not have leading zeros
        return None;
    }
    let mut x: u32 = 0;
    for c in data {
        if b'0' <= *c && *c <= b'9' {
            x = x.checked_mul(10)?.checked_add((*c - b'0') as u32)?;
        } else {
            return None;
        }
    }
    Some(x)
}

/// Read the XBM file header up to and including the first opening brace
fn read_xbm_header<'a, R: Iterator<Item = u8>>(
    r: &mut TextReader<R>,
    name_width_buf: &'a mut [u8],
) -> Result<(&'a [u8], XbmHeaderData), XbmDecodeError> {
    // The header consists of three to five lines. Lines 3-4 may be skipped
    // In practice, the name may be empty or UTF-8.
    //
    //  #define <name>_width <width>
    //  #define <name>_height <height>
    //  #define <name>_x_hot <x>
    //  #define <name>_y_hot <y>
    //  static <type> <name>_bits[] = { ...
    let mut int_buf = [0u8; 11]; // -2^31 and 2^32 fit in 11 bytes

    // Read width field and acquire name.
    read_fixed_string(r, b"#define", XbmPart::Width)?;
    read_whitespace_gap(r, XbmPart::Width)?;
    let name_width = read_until_whitespace(r, name_width_buf, XbmPart::Width)?;
    if !name_width.ends_with(b"_width") {
        return Err(XbmDecodeError::Parse(XbmPart::Width, r.loc()));
    }
    let name = &name_width[..name_width.len() - b"_width".len()];
    read_whitespace_gap(r, XbmPart::Width)?;
    let int = read_until_whitespace(r, &mut int_buf, XbmPart::Width)?;
    read_to_newline(r, XbmPart::Width)?;

    let width = parse_u32(int).ok_or(XbmDecodeError::DecodeInteger(XbmPart::Width))?;
    if width == 0 {
        return Err(XbmDecodeError::ZeroWidth);
    }

    // Read height field, checking that the name matches
    read_fixed_string(r, b"#define", XbmPart::Height)?;
    read_whitespace_gap(r, XbmPart::Height)?;
    read_fixed_string(r, name, XbmPart::Height)?;
    read_fixed_string(r, b"_height", XbmPart::Height)?;
    read_whitespace_gap(r, XbmPart::Height)?;
    let int = read_until_whitespace(r, &mut int_buf, XbmPart::Height)?;
    read_to_newline(r, XbmPart::Height)?;

    let height = parse_u32(int).ok_or(XbmDecodeError::DecodeInteger(XbmPart::Height))?;
    if height == 0 {
        return Err(XbmDecodeError::ZeroHeight);
    }

    let hotspot = match r.peek() {
        Some(b'#') => {
            // Parse hotspot lines
            read_fixed_string(r, b"#define", XbmPart::HotspotX)?;
            read_whitespace_gap(r, XbmPart::HotspotX)?;
            read_fixed_string(r, name, XbmPart::HotspotX)?;
            read_fixed_string(r, b"_x_hot", XbmPart::HotspotX)?;
            read_whitespace_gap(r, XbmPart::HotspotX)?;
            let int = read_until_whitespace(r, &mut int_buf, XbmPart::HotspotX)?;
            read_to_newline(r, XbmPart::HotspotX)?;

            let hotspot_x =
                parse_i32(int).ok_or(XbmDecodeError::DecodeInteger(XbmPart::HotspotX))?;

            read_fixed_string(r, b"#define", XbmPart::HotspotY)?;
            read_whitespace_gap(r, XbmPart::HotspotY)?;
            read_fixed_string(r, name, XbmPart::HotspotY)?;
            read_fixed_string(r, b"_y_hot", XbmPart::HotspotY)?;
            read_whitespace_gap(r, XbmPart::HotspotY)?;
            let int = read_until_whitespace(r, &mut int_buf, XbmPart::HotspotY)?;
            read_to_newline(r, XbmPart::HotspotY)?;

            let hotspot_y =
                parse_i32(int).ok_or(XbmDecodeError::DecodeInteger(XbmPart::HotspotY))?;

            Some((hotspot_x, hotspot_y))
        }
        Some(b's') => None,
        _ => {
            r.next();
            return Err(XbmDecodeError::Parse(XbmPart::Array, r.loc()));
        }
    };

    read_fixed_string(r, b"static", XbmPart::Array)?;
    read_whitespace_gap(r, XbmPart::Array)?;
    match r.peek() {
        Some(b'c') => {
            read_fixed_string(r, b"char", XbmPart::Array)?;
        }
        Some(b'u') => {
            read_fixed_string(r, b"unsigned", XbmPart::Array)?;
            read_whitespace_gap(r, XbmPart::Array)?;
            read_fixed_string(r, b"char", XbmPart::Array)?;
        }
        _ => {
            r.next();
            return Err(XbmDecodeError::Parse(XbmPart::Array, r.loc()));
        }
    }
    read_whitespace_gap(r, XbmPart::Array)?;
    read_fixed_string(r, name, XbmPart::Array)?;
    read_fixed_string(r, b"_bits[]", XbmPart::Array)?;
    read_whitespace_gap(r, XbmPart::Array)?;
    read_fixed_string(r, b"=", XbmPart::Array)?;
    read_whitespace_gap(r, XbmPart::Array)?;
    read_fixed_string(r, b"{", XbmPart::Array)?;

    Ok((
        name,
        XbmHeaderData {
            width,
            height,
            hotspot,
        },
    ))
}

impl<R> XbmStreamDecoder<R>
where
    R: Iterator<Item = u8>,
{
    /// Create a new `XbmStreamDecoder` or error if the header failed to parse.
    pub fn new(reader: R) -> Result<XbmStreamDecoder<R>, (R, XbmDecodeError)> {
        let mut r = TextReader::new(reader);

        let mut name_width_buf = [0u8; MAX_IDENTIFIER_LENGTH];
        match read_xbm_header(&mut r, &mut name_width_buf) {
            Err(e) => Err((r.inner, e)),
            Ok((_name, header)) => Ok(XbmStreamDecoder {
                r,
                current_position: 0,
                header,
            }),
        }
    }

    /// Read the next byte of the raw image data. The XBM image is organized
    /// in row major order with rows containing ceil(width / 8) bytes, so that
    /// the `i`th pixel in a row is the `(i%8)`th least significant
    /// bit of the `(i/8)`th byte in the row. Bit value 1 = black, 0 = white.
    pub fn next_byte(&mut self) -> Result<Option<u8>, XbmDecodeError> {
        let data_size = (self.header.width.div_ceil(8) as u64) * (self.header.height as u64);
        if self.current_position < data_size {
            let first = self.current_position == 0;
            self.current_position += 1;

            if !first {
                read_optional_whitespace(&mut self.r)?;
                read_fixed_string(&mut self.r, b",", XbmPart::Data)?;
            }
            read_optional_whitespace(&mut self.r)?;
            Ok(Some(read_hex_byte(&mut self.r, XbmPart::Data)?))
        } else {
            // Read optional comma, followed by final };
            read_optional_whitespace(&mut self.r)?;
            match self.r.peek() {
                Some(b',') => {
                    read_fixed_string(&mut self.r, b",", XbmPart::Data)?;
                    read_optional_whitespace(&mut self.r)?;
                }
                Some(b'}') => (),
                _ => {
                    self.r.next();
                    return Err(XbmDecodeError::Parse(XbmPart::ArrayEnd, self.r.loc()));
                }
            }
            read_fixed_string(&mut self.r, b"}", XbmPart::ArrayEnd)?;
            read_optional_whitespace(&mut self.r)?;
            read_fixed_string(&mut self.r, b";", XbmPart::ArrayEnd)?;
            read_optional_whitespace(&mut self.r)?;

            if self.r.next().is_some() {
                // File has unexpected trailing contents
                return Err(XbmDecodeError::Parse(XbmPart::Trailing, self.r.loc()));
            };

            Ok(None)
        }
    }
}

impl<R> XbmDecoder<R>
where
    R: BufRead,
{
    /// Create a new `XBMDecoder`.
    pub fn new(reader: R) -> Result<XbmDecoder<R>, ImageError> {
        match XbmStreamDecoder::new(IoAdapter {
            reader: reader.bytes(),
            error: None,
        }) {
            Err((mut r, e)) => Err(e).apply_after(&mut r.error),
            Ok(x) => Ok(XbmDecoder { base: x }),
        }
    }

    /// Returns the (x,y) hotspot coordinates of the image, if the image provides them.
    pub fn hotspot(&self) -> Option<(i32, i32)> {
        self.base.header.hotspot
    }
}

impl<R: BufRead> ImageDecoder for XbmDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.base.header.width, self.base.header.height)
    }
    fn color_type(&self) -> ColorType {
        ColorType::L8
    }
    fn original_color_type(&self) -> ExtendedColorType {
        ExtendedColorType::L1
    }
    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()>
    where
        Self: Sized,
    {
        for row in buf.chunks_exact_mut(self.base.header.width as usize) {
            // The XBM format discards the last `8 * ceil(self.width / 8) - self.width` bits in each row
            for chunk in row.chunks_mut(8) {
                let nxt = self
                    .base
                    .next_byte()
                    .apply_after(&mut self.base.r.inner.error)?;
                let val = nxt.ok_or_else(|| {
                    ImageError::Parameter(ParameterError::from_kind(
                        ParameterErrorKind::DimensionMismatch,
                    ))
                })?;
                for (i, p) in chunk.iter_mut().enumerate() {
                    // Set bits correspond to black, unset bits to white
                    *p = if val & (1 << i) == 0 { 0xff } else { 0 };
                }
            }
        }

        let val = self
            .base
            .next_byte()
            .apply_after(&mut self.base.r.inner.error)?;
        if val.is_some() {
            return Err(ImageError::Parameter(ParameterError::from_kind(
                ParameterErrorKind::DimensionMismatch,
            )));
        }

        Ok(())
    }
    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::BufReader;

    #[test]
    fn image_without_hotspot() {
        let decoder = XbmDecoder::new(BufReader::new(
            File::open("tests/images/xbm/1x1.xbm").unwrap(),
        ))
        .expect("Unable to read XBM file");

        assert_eq!((1, 1), decoder.dimensions());
        assert_eq!(None, decoder.hotspot());
    }

    #[test]
    fn image_with_hotspot() {
        let decoder = XbmDecoder::new(BufReader::new(
            File::open("tests/images/xbm/hotspot.xbm").unwrap(),
        ))
        .expect("Unable to read XBM file");

        assert_eq!((5, 5), decoder.dimensions());
        assert_eq!(Some((-1, 2)), decoder.hotspot());
    }
}

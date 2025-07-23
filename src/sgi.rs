/*! Decoding of SGI Image File Format (.rgb)
 *
 * The SGI Image File format (often referred to as .rgb) is an obsolete
 * file format which has uncompressed and run-length encoded modes,
 * supports a variable number of color channels, and both 8-bit and
 * 16-bit precisions.
 *
 * This decoder does not support:
 * - Images with ≥ 5 color channels (zsize). (While theoretically supported
 *   by the format, we haven't found any existing images that do this.)
 *
 * - Colormaps: Not supported, only the NORMAL=0 mode. The other operations
 *   (DITHERED=1, SCREEN=2, COLORMAP=3) were obsolete at the time the spec
 *   was written.
 *
 * The format specification does not explain how the alpha channel is to
 * be interpreted. Existing decoders appear to assume straight alpha, like
 * PNG.
 *
 * Specification:
 * - <https://web.archive.org/web/20010413154909/https://reality.sgi.com/grafica/sgiimage.html>
 * - <https://www.fileformat.info/format/sgiimage/egff.htm>
 *
 */

use core::ffi::CStr;
use std::fmt;
use std::io::BufRead;

use image::error::{
    DecodingError, ImageError, ImageFormatHint, ImageResult, LimitError, LimitErrorKind,
};
use image::{ColorType, ExtendedColorType, ImageDecoder, LimitSupport, Limits};

/// The length of the SGI .rgb file header, including padding
const HEADER_FULL_LENGTH: usize = 512;

/// Important information from an SGI Image header.
/// This _excludes_:
/// - The name field (returned separately)
/// - the pixmin/pixmax fields, which different encoders generate
///   inconsistently and are easy to compute if needed
/// - colormap field: only colormap NORMAL=0 is accepted, not obsolete modes
///
#[derive(Clone, Copy)]
struct SgiRgbHeaderInfo {
    xsize: u16,
    ysize: u16,
    color_type: ColorType,
    is_rle: bool,
}

/// Errors which can occur while parsing an SGI .rgb file
#[derive(Debug)]
enum SgiRgbDecodeError {
    BadMagic,
    HeaderError,
    RLERowInvalid(u32, u32), // Invalid RLE row specification
    UnexpectedColormap(u32),
    UnexpectedZSize(u16),
    ZeroSize(u16, u16, u16),
    ScanlineOverflow,
    ScanlineUnderflow,
    ScanlineTooShort,
    ScanlineTooLong,
    InvalidName,
    EarlyEOF,
}

impl fmt::Display for SgiRgbDecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadMagic => f.write_str("Incorrect magic bytes, not an SGI image file"),
            Self::HeaderError => f.write_str("Error parsing header"),
            Self::UnexpectedColormap(code) => {
                f.write_fmt(format_args!("Unexpected color map value ({})", code))
            }
            Self::UnexpectedZSize(dim) => {
                f.write_fmt(format_args!("Unexpected Z dimension is >= 5 ({})", dim))
            }
            Self::ZeroSize(x, y, z) => f.write_fmt(format_args!(
                "Image has zero size: x*y*z={}*{}*{}=0",
                x, y, z
            )),
            Self::RLERowInvalid(offset, length) => {
                f.write_fmt(format_args!("Bad RLE row info (offset={}, length={}); empty or not in image data region", offset, length))
            }
            Self::InvalidName => {
                f.write_str("Invalid name field (not null terminated or not ASCII)")
            }
            Self::ScanlineOverflow => {
                f.write_str("An RLE-encoded scanline contained more data than the image width")
            }
            Self::ScanlineUnderflow => {
                f.write_str("An RLE-encoded scanline contained less data than the image width")
            }
            Self::ScanlineTooShort => {
                f.write_str("An RLE-encoded scanline stopped (reached a zero counter) before its stated length")
            }
            Self::ScanlineTooLong => {
                f.write_str("An RLE-encoded scanline did not stop at its stated length (missing trailing zero counter).")
            }
            Self::EarlyEOF => {
                f.write_str("File ended before all scanlines were read.")
            }
        }
    }
}

impl std::error::Error for SgiRgbDecodeError {}

impl From<SgiRgbDecodeError> for ImageError {
    fn from(e: SgiRgbDecodeError) -> ImageError {
        ImageError::Decoding(DecodingError::new(ImageFormatHint::Name("SGI".into()), e))
    }
}

/// Parse the image header, returning key metadata and the name field if successful
fn parse_header(
    header: &[u8; HEADER_FULL_LENGTH],
) -> Result<(SgiRgbHeaderInfo, &CStr), SgiRgbDecodeError> {
    if header[..2] != *b"\x01\xda" {
        return Err(SgiRgbDecodeError::BadMagic);
    }
    let storage = header[2];
    let bpc = header[3];
    let dimension = u16::from_be_bytes(header[4..6].try_into().unwrap());
    let xsize = u16::from_be_bytes(header[6..8].try_into().unwrap());
    let ysize = u16::from_be_bytes(header[8..10].try_into().unwrap());
    let zsize = u16::from_be_bytes(header[10..12].try_into().unwrap());
    let _pixmin = u32::from_be_bytes(header[12..16].try_into().unwrap());
    let _pixmax = u32::from_be_bytes(header[16..20].try_into().unwrap());
    /* Dummy bytes -- formerly, this was a 'wastebytes' field, so old
     * images may not set this to zero. */
    let _dummy1 = u32::from_be_bytes(header[20..24].try_into().unwrap());
    let imagename: &[u8] = &header[24..104];
    let colormap = u32::from_be_bytes(header[104..108].try_into().unwrap());
    /* More dummy bytes -- old libraries appear to have used the same struct
     * for header data and, following it, internal image parsing state; and
     * when writing the header just wrote whatever was in the struct. As a
     * result, the following bytes may contain random counters, pointer data,
     * etc. in old images. Recently (post 2000) written encoders tend to
     * follow the file format specification and write zeros here. */
    let _dummy2 = &header[108..];
    if storage >= 2 {
        return Err(SgiRgbDecodeError::HeaderError);
    }
    if bpc == 0 {
        return Err(SgiRgbDecodeError::HeaderError);
    }
    if colormap != 0 {
        return Err(SgiRgbDecodeError::UnexpectedColormap(colormap));
    }
    // Header fields `ysize`/`zsize` are only validated if used
    let (xsize, ysize, zsize) = match dimension {
        0 | 4.. => {
            return Err(SgiRgbDecodeError::HeaderError);
        }
        1 => (xsize, 1, 1),
        2 => (xsize, ysize, 1),
        3 => (xsize, ysize, zsize),
    };
    if xsize == 0 || ysize == 0 || zsize == 0 {
        return Err(SgiRgbDecodeError::ZeroSize(xsize, ysize, zsize));
    }
    let name = CStr::from_bytes_until_nul(imagename).map_err(|_| SgiRgbDecodeError::InvalidName)?;
    if name.to_bytes().iter().any(|x| !x.is_ascii()) {
        return Err(SgiRgbDecodeError::InvalidName);
    }

    let color_type = match (bpc, zsize) {
        (1, 1) => ColorType::L8,
        (1, 2) => ColorType::La8,
        (1, 3) => ColorType::Rgb8,
        (1, 4) => ColorType::Rgba8,
        (2, 1) => ColorType::L16,
        (2, 2) => ColorType::La16,
        (2, 3) => ColorType::Rgb16,
        (2, 4) => ColorType::Rgba16,
        _ => {
            if zsize >= 5 {
                return Err(SgiRgbDecodeError::UnexpectedZSize(zsize));
            } else {
                return Err(SgiRgbDecodeError::HeaderError);
            }
        }
    };
    Ok((
        SgiRgbHeaderInfo {
            is_rle: storage == 1,
            xsize,
            ysize,
            color_type,
        },
        name,
    ))
}

/// Decoder for SGI (.rgb) images.
pub struct SgiDecoder<R>
where
    R: BufRead,
{
    info: SgiRgbHeaderInfo,
    reader: R,
}

impl<R> SgiDecoder<R>
where
    R: BufRead,
{
    /// Create a new `SgiDecoder`. Assumes `r` starts at seek position 0.
    pub fn new(mut r: R) -> Result<SgiDecoder<R>, ImageError> {
        let mut header = [0u8; HEADER_FULL_LENGTH];
        r.read_exact(&mut header)?;
        let (header_info, _name) = parse_header(&header)?;

        Ok(SgiDecoder {
            info: header_info,
            reader: r,
        })
    }
}

/** State associated with the processing of a scan line */
#[derive(Clone, Copy)]
struct SgiRgbScanlineState {
    /// Offset of the encoded row in the input file
    offset: u32,
    /// Length of the encoded row in the file, including terminator
    length: u32,
    /// Current row number in 0..ysize
    row_id: u16,
    /// Current position to write in the current image row, range 0..=xsize
    position: u16,
    /// Index of the preceding line which contains the current file position
    /// (or u32::MAX if there is no such line.) Other than u32::MAX, values
    /// range from 0 to 2^18 - 1
    prec_active_line: u32,
    /// Color plane of row. Values in 0..4
    plane: u8,
    /// The most recently read counter byte; for copy operations, this may be
    /// modified as the copy progresses
    counter: u8,
    /// If 16-bit depth used, last high byte read
    data_char_hi: u8,
    /// If true, will next read a counter field
    at_counter: bool,
    /// If true, will next read a high byte
    high_byte: bool,
}

/** State associated with the processing of the image data for RLE type images */
struct SgiRgbDecodeState {
    /// Number of bytes in the stream read so far, including header. In practice,
    /// this can be as large as the max end of a valid encoded scanline (2^32 + 2^18 - 3)
    pos: u64,
    /// Maximum index of a row which contains pos, if one exists.
    max_active_row: Option<u32>,
    /// Index of the next row in the sequence which has offset >= pos, or rle_table.len() if no such row
    cursor: usize,
}

/** Apply a byte of the input to a scanline state machine.
 * Returns `Ok(true)` if this byte completes an end of line marker.
 * Returns Err(true) on line overflow, Err(false) on line underflow. */
fn apply_rle16_byte(
    row: &mut SgiRgbScanlineState,
    buf_row: &mut [u8],
    byte: u8,
    xsize: u16,
    channels: u8,
) -> Result<bool, bool> {
    let mut eor = false;
    if row.high_byte {
        row.data_char_hi = byte;
        row.high_byte = false;
    } else {
        if row.at_counter {
            row.counter = byte;
            row.at_counter = false;
            let count = row.counter & (!0x80);
            if count == 0 {
                // End of line
                if row.position != xsize {
                    return Err(false);
                } else {
                    eor = true;
                }
            }
        } else {
            let data = u16::from_be_bytes([row.data_char_hi, byte]);
            let mut count = row.counter & (!0x80);
            /* Have checked that count != 0 */
            if count == 0 {
                unreachable!();
            } else if row.counter & 0x80 == 0 {
                // Expand data to `count` repeated u16s
                let overflows_row = (count as u16) > xsize || row.position > xsize - (count as u16);
                if overflows_row {
                    return Err(true);
                }
                // The calculated write_start should be <= buf.len() and hence not overflow
                let write_start: usize = (row.position as usize) * (channels as usize);
                for i in 0..count {
                    let o = write_start + (row.plane as usize) + (channels as usize) * (i as usize);
                    buf_row[2 * o..2 * o + 2].copy_from_slice(&u16::to_ne_bytes(data));
                }
                row.position += count as u16;
                row.at_counter = true;
            } else {
                // Copy the next `count` u16s of data
                let overflows_row = row.position > xsize - 1;
                if overflows_row {
                    return Err(true);
                }

                let write_start: usize =
                    (row.position as usize) * (channels as usize) + (row.plane as usize);
                buf_row[2 * write_start..2 * write_start + 2]
                    .copy_from_slice(&u16::to_ne_bytes(data));

                count -= 1;
                row.position += 1;
                row.counter = 0x80 | count;
                row.at_counter = count == 0;
            }
        }

        row.high_byte = !row.high_byte;
    }
    Ok(eor)
}

/** Apply a byte of the input to a scanline state machine.
 * Returns Ok(true) if this byte completes an end of line marker.
 * Returns Err(true) on line overflow, Err(false) on line underflow. */
fn apply_rle8_byte(
    row: &mut SgiRgbScanlineState,
    buf_row: &mut [u8],
    byte: u8,
    xsize: u16,
    channels: u8,
) -> Result<bool, bool> {
    let mut eor = false;
    if row.at_counter {
        row.counter = byte;
        row.at_counter = false;
        let count = row.counter & (!0x80);
        if count == 0 {
            // End of line
            if row.position != xsize {
                return Err(false);
            } else {
                eor = true;
            }
        }
    } else {
        let mut count = row.counter & (!0x80);
        /* Have checked that count != 0 */
        if count == 0 {
            unreachable!();
        } else if row.counter & 0x80 == 0 {
            // Expand data to `count` repeated u16s
            let overflows_row = (count as u16) > xsize || row.position > xsize - (count as u16);
            if overflows_row {
                return Err(true);
            }
            // The calculated write_start should be <= buf.len() and hence not overflow
            let write_start: usize = (row.position as usize) * (channels as usize);
            for i in 0..count {
                let o = write_start + (row.plane as usize) + (channels as usize) * (i as usize);
                buf_row[o] = byte;
            }
            row.position += count as u16;
            row.at_counter = true;
        } else {
            // Copy the next `count` u8s of data
            let overflows_row = row.position > xsize - 1;
            if overflows_row {
                return Err(true);
            }

            let write_start: usize =
                (row.position as usize) * (channels as usize) + (row.plane as usize);
            buf_row[write_start] = byte;

            count -= 1;
            row.position += 1;
            row.counter = 0x80 | count;
            row.at_counter = count == 0;
        }
    }
    Ok(eor)
}

/** For the given RLE scanline, process the bytes in `segment`.
 * Parameter `ending` is true iff this segment ends the scanline. */
fn process_rle_segment<const DEEP: bool>(
    buf_row: &mut [u8],
    row: &mut SgiRgbScanlineState,
    xsize: u16,
    channels: u8,
    segment: &[u8],
    ending: bool,
) -> Result<(), SgiRgbDecodeError> {
    for (i, byte) in segment.iter().enumerate() {
        let res = if DEEP {
            apply_rle16_byte(row, buf_row, *byte, xsize, channels)
        } else {
            apply_rle8_byte(row, buf_row, *byte, xsize, channels)
        };
        let eor = res.map_err(|overflow| {
            if overflow {
                SgiRgbDecodeError::ScanlineOverflow
            } else {
                SgiRgbDecodeError::ScanlineUnderflow
            }
        })?;

        let expecting_end = i + 1 == segment.len() && ending;
        if eor != expecting_end {
            if eor {
                // Row ended earlier than expected
                return Err(SgiRgbDecodeError::ScanlineTooShort);
            } else {
                // At end of scanline data, did not process a row end marker
                return Err(SgiRgbDecodeError::ScanlineTooLong);
            }
        }
    }

    Ok(())
}

/** Process the next region of the RLE-encoded image data section of the SGI image file */
fn process_data_segment<const DEEP: bool>(
    buf: &mut [u8],
    info: SgiRgbHeaderInfo,
    mut state: SgiRgbDecodeState,
    rle_table: &mut [SgiRgbScanlineState],
    segment: &[u8],
) -> Result<(SgiRgbDecodeState, bool), SgiRgbDecodeError> {
    let channels = info.color_type.channel_count();
    let start_pos = state.pos;
    let end_pos = state.pos + segment.len() as u64;

    // Add new encoded lines that intersect `segment` to the active set
    while state.cursor < rle_table.len() && rle_table[state.cursor].offset as u64 <= end_pos {
        rle_table[state.cursor].prec_active_line = if let Some(r) = state.max_active_row {
            r
        } else {
            u32::MAX
        };
        state.max_active_row = Some(state.cursor as u32);
        state.cursor += 1;
    }

    let mut prev_row: Option<u32> = None;
    let Some(mut row_index) = state.max_active_row else {
        // No rows are active, nothing to do
        state.pos = end_pos;
        return Ok((state, false));
    };

    loop {
        let row = &mut rle_table[row_index as usize];
        let row_end = row.offset as u64 + row.length as u64;
        debug_assert!(row.offset as u64 <= end_pos && row_end > start_pos);

        let prev_active_row = row.prec_active_line;

        // Intersect the segment being processed with the row extents, and then
        // run the RLE state machine over the resulting intersected segment
        let rs_start: usize = if row.offset as u64 > start_pos {
            ((row.offset as u64) - start_pos) as usize
        } else {
            0
        };
        let rs_end: usize = if row_end <= end_pos {
            (row_end - start_pos) as usize
        } else {
            segment.len()
        };
        let has_ending = row_end <= end_pos;
        let row_input = &segment[rs_start..rs_end];

        let bpp = if DEEP { 2 } else { 1 };
        let stride = (info.xsize as usize) * (channels as usize) * bpp;
        let buf_row = &mut buf[((info.ysize - 1 - row.row_id) as usize) * stride
            ..((info.ysize - 1 - row.row_id) as usize) * stride + stride];

        process_rle_segment::<DEEP>(buf_row, row, info.xsize, channels, row_input, has_ending)?;

        if has_ending {
            // Mark this row as not having a preceding row, and remove
            // it from the singly linked list
            row.prec_active_line = u32::MAX;
            if let Some(r) = prev_row {
                rle_table[r as usize].prec_active_line = prev_active_row;
            } else if prev_active_row == u32::MAX {
                state.max_active_row = None;
            } else {
                state.max_active_row = Some(prev_active_row);
            }
        } else {
            prev_row = Some(row_index);
        }

        if prev_active_row != u32::MAX {
            row_index = prev_active_row;
        } else {
            break;
        }
    }

    state.pos = end_pos;
    let done = state.max_active_row.is_none() && state.cursor == rle_table.len();
    Ok((state, done))
}

impl<R: BufRead> ImageDecoder for SgiDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.info.xsize as u32, self.info.ysize as u32)
    }

    fn color_type(&self) -> ColorType {
        self.info.color_type
    }

    fn original_color_type(&self) -> ExtendedColorType {
        self.info.color_type.into()
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert_eq!(u64::try_from(buf.len()), Ok(self.total_bytes()));

        let channels = self.info.color_type.channel_count();
        let deep = self.info.color_type.bytes_per_pixel() > channels;
        if self.info.is_rle {
            /* Tricky case: need to read the RLE offset tables to determine
             * where to find the scanlines for each plane. Images can place
             * the scanlines whereever, and processing them in logical order
             * can require a lot of seeking.
             *
             * This decoder does not have fine grained control over the
             * input stream buffering, so each seek operation could make
             * the input stream read 8 kB and/or make a syscall if the
             * input stream is a default BufReader. An image could trigger
             * one seek operation per 8 marginal bytes of data. To avoid
             * this unfortunate interaction, we load the O(height) bytes
             * of RLE tables into memory and then process the rest of the
             * image in a single pass.
             */

            /* `rle_offset_entries` has maximum value `4 * (2^16-1)` and will not overflow */
            let rle_offset_entries = (channels as u32) * (self.info.ysize as u32);

            let mut rle_table: Vec<SgiRgbScanlineState> = Vec::new();
            /* Tiny invalid images can trigger medium-size 4 * (2^16-1) allocations here;
             * this can be avoided by dynamically resizing the vector as it is read. */
            rle_table
                .try_reserve_exact(rle_offset_entries as usize)
                .map_err(|_| ImageError::Limits(LimitErrorKind::InsufficientMemory.into()))?;
            rle_table.resize(
                rle_offset_entries as usize,
                SgiRgbScanlineState {
                    offset: 0,
                    length: 0,
                    row_id: 0,
                    position: 0,
                    plane: 0,
                    counter: 0,
                    data_char_hi: 0,
                    prec_active_line: 0,
                    high_byte: deep,
                    at_counter: true,
                },
            );
            // Read offset table
            for plane in 0..channels {
                for y in 0..self.info.ysize {
                    let idx = (plane as usize) * (self.info.ysize as usize) + (y as usize);
                    let mut tmp = [0u8; 4];
                    self.reader.read_exact(&mut tmp)?;
                    rle_table[idx].offset = u32::from_be_bytes(tmp);
                    rle_table[idx].row_id = y;
                    rle_table[idx].plane = plane;
                }
            }
            // Read length table, and validate (offset, length) pairs
            for plane in 0..channels {
                for y in 0..self.info.ysize {
                    let idx = (plane as usize) * (self.info.ysize as usize) + (y as usize);
                    let mut tmp = [0u8; 4];
                    self.reader.read_exact(&mut tmp)?;
                    rle_table[idx].length = u32::from_be_bytes(tmp);

                    // Per spec, image data follows the offset tables.
                    // (although other decoders will probably read inside the offset tables
                    // if asked.)
                    let scanline_too_early = rle_table[idx].offset
                        < (HEADER_FULL_LENGTH as u32) + rle_offset_entries * 8;
                    let zero_length = rle_table[idx].length == 0;

                    if scanline_too_early || zero_length {
                        return Err(SgiRgbDecodeError::RLERowInvalid(
                            rle_table[idx].offset,
                            rle_table[idx].length,
                        )
                        .into());
                    }
                }
            }

            // Sort rows by their starting position in the stream, breaking
            // ties by their position in the buffer.
            rle_table.sort_unstable_by_key(|f| {
                (f.offset as u64) << 32 | (f.row_id as u64) << 16 | f.plane as u64
            });

            // Use explicit state for linear processing
            let mut rle_state = SgiRgbDecodeState {
                cursor: 0,
                max_active_row: None,
                pos: (HEADER_FULL_LENGTH as u64) + (rle_table.len() as u64) * 8,
            };

            loop {
                let buffer = self.reader.fill_buf()?;
                if buffer.is_empty() {
                    /* Unexpected EOF . */
                    return Err(SgiRgbDecodeError::EarlyEOF.into());
                }

                let (new_state, done) = if deep {
                    process_data_segment::<true>(buf, self.info, rle_state, &mut rle_table, buffer)?
                } else {
                    process_data_segment::<false>(
                        buf,
                        self.info,
                        rle_state,
                        &mut rle_table,
                        buffer,
                    )?
                };
                if done {
                    return Ok(());
                }
                rle_state = new_state;

                let buffer_length = buffer.len();
                self.reader.consume(buffer_length);
            }
        } else {
            /* Easy case: packed images by scanline, plane by plane  */
            if deep {
                let bpp = 2 * channels;
                // `width` will be at most `(2^16-1) * 8`, so there is never overflow
                let width = (bpp as u32) * (self.info.xsize as u32);
                for plane in 0..channels as usize {
                    for row in buf.chunks_exact_mut(width as usize).rev() {
                        for px in row.chunks_exact_mut(bpp as usize) {
                            let mut tmp = [0_u8; 2];
                            self.reader.read_exact(&mut tmp)?;
                            px[2 * plane..2 * plane + 2]
                                .copy_from_slice(&u16::to_ne_bytes(u16::from_be_bytes(tmp)));
                        }
                    }
                }
            } else {
                let width = (channels as u32) * (self.info.xsize as u32);
                for plane in 0..channels as usize {
                    for row in buf.chunks_exact_mut(width as usize).rev() {
                        for px in row.chunks_exact_mut(channels as usize) {
                            self.reader.read_exact(&mut px[plane..plane + 1])?;
                        }
                    }
                }
            }
            Ok(())
        }
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }

    fn set_limits(&mut self, limits: Limits) -> ImageResult<()> {
        limits.check_support(&LimitSupport::default())?;
        let (width, height) = self.dimensions();
        limits.check_dimensions(width, height)?;

        // This will not overflow, because 8 * (2^16-1) * (2^16-1) ≤ 2^35
        let max_image_bytes = 8 * (self.info.xsize as u64) * (self.info.ysize as u64);
        // This will not overflow, because even 1 KB * (2^16-1) ≤ 2^35 ⪡ 2^64-1
        let max_table_bytes =
            (self.info.ysize as u64) * (std::mem::size_of::<SgiRgbScanlineState>() as u64);
        // This will not overflow, because it is ≤ 2^36 ⪡ 2^64-1
        let max_bytes = max_image_bytes + max_table_bytes;

        let max_alloc = limits.max_alloc.unwrap_or(u64::MAX);
        if max_alloc < max_bytes {
            return Err(ImageError::Limits(LimitError::from_kind(
                LimitErrorKind::InsufficientMemory,
            )));
        }
        Ok(())
    }
}

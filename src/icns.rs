//! Decoding of ICNS image files
//!
//! The .icns (Apple Icon Image format) format is an icon format used by macOS.
//!
//! Similar to the `image` crate's .ico decoder, this decoder just extracts the
//! "best" contained image in the file (largest, highest bit depth, earliest
//! option.)
//!
//! Nested icns files are ignored, as may be unsupported icon image types.
//!
//! This implementation does not include support for decoding JPEG 2000
//! subimages, but [IcnsDecoder::new_with_decode_func] can be used to provide
//! your own implementation. The default image format hook registered in
//! [crate::register] will, if the "best" image entry happens to use JPEG 2000,
//! try to decode the subimage using the image format hook registered for JP2
//! images, if one has been set up.
//!
//! # Related Links
//! * <https://en.wikipedia.org/wiki/Apple_Icon_Image_format> - ICNS format on Wikipedia
//! * <https://web.archive.org/web/20180618155438/https://developer.apple.com/design/human-interface-guidelines/macos/icons-and-images/app-icon/> - ICNS is no longer recommended for macOS icons

use std::collections::HashMap;
use std::fmt::{self, Display};
use std::io::{Cursor, Read, Seek, SeekFrom};

use image::error::{
    DecodingError, ImageFormatHint, LimitError, LimitErrorKind, UnsupportedError,
    UnsupportedErrorKind,
};
use image::{ColorType, ImageDecoder, ImageError, ImageReader, ImageResult, LimitSupport, Limits};

use icns::{Encoding, IconElement, IconType, OSType, PixelFormat};

/// Errors that can occur during decoding and parsing an ICO image or one of its enclosed images.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
enum DecoderError {
    /// The end of the image occurs at a stream position > u64::MAX
    ImageEndAfterEndOfStream,
    /// The icon file does not begin with b"icns"
    NotICNS,
    /// The icon file contains multiple image entries of a given type
    DuplicateEntry(IconType),
    /// The last record in the file does not end exactly at the ICNS file end point
    IncompleteEntry,
    /// Did not find a decodable image entry
    NoImageFound,
    /// The given entry type requires a corresponding mask element, but none was found
    MissingMask(IconType),
    /// Image entry length field impossibly short
    BadEntryLength,
    /// An image entry, expected to be have either PNG or Jpeg 2000 content, had neither
    NotPNGorJP2(IconType),
    /// An image entry with PNG contents had size inconsistent with the icon
    BadPngSize(u32, u32, u32),
    /// An image entry with JP2 contents had size inconsistent with the icon
    BadJp2Size(u32, u32, u32),
}

impl From<DecoderError> for ImageError {
    fn from(e: DecoderError) -> ImageError {
        ImageError::Decoding(DecodingError::new(icns_format_hint(), e))
    }
}

impl Display for DecoderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecoderError::ImageEndAfterEndOfStream => {
                f.write_str("The end of the image would have a stream position >u64::MAX")
            }
            DecoderError::NotICNS => f.write_str("Image file does not start with 'icns'"),
            DecoderError::DuplicateEntry(t) => f.write_fmt(format_args!(
                "Image file contains multiple entries of type {t:?}"
            )),
            DecoderError::MissingMask(t) => f.write_fmt(format_args!(
                "Image file did not contain a mask entry for the entry of type {t:?}"
            )),
            DecoderError::IncompleteEntry => f.write_str(
                "The last entry in the file would extend past the end of file indicated by the header"
            ),
            DecoderError::NoImageFound => f.write_str(
                "No image entry that the decoder might support was found"
            ),
            DecoderError::BadEntryLength => f.write_str(
                "Image file contained an entry with invalid length (less than 8)"
            ),
            DecoderError::NotPNGorJP2(t) => f.write_fmt(format_args!(
                "Image file entry of type {t:?} contained neither PNG nor Jpeg 2000 data"
            )),
            DecoderError::BadPngSize(w,h,s) => f.write_fmt(format_args!(
                "Image file entry with PNG data had size {w}x{h} instead of expected {s}x{s}"
            )),
            DecoderError::BadJp2Size(w,h,s) => f.write_fmt(format_args!(
                "Image file entry with Jpeg 2000 data had size {w}x{h} instead of expected {s}x{s}"
            )),
        }
    }
}
impl std::error::Error for DecoderError {}

fn icns_format_hint() -> ImageFormatHint {
    ImageFormatHint::Name("ICNS".into())
}

/// Adapt PNG decoding errors to ImageError, adding an ICNS format hint when possible
fn png_to_image_error(err: png::DecodingError) -> ImageError {
    use png::DecodingError::*;
    match err {
        IoError(err) => ImageError::IoError(err),
        // The input image was not a valid PNG.
        err @ Format(_) => ImageError::Decoding(DecodingError::new(icns_format_hint(), err)),
        Parameter(_) => unreachable!(),
        LimitsExceeded => {
            ImageError::Limits(LimitError::from_kind(LimitErrorKind::InsufficientMemory))
        }
    }
}

/// When possible, wrap the error passed in with an ICNS format hint.
fn jp2_to_image_error(err: ImageError) -> ImageError {
    match err {
        ImageError::Decoding(e) => ImageError::Decoding(DecodingError::new(icns_format_hint(), e)),
        ImageError::Encoding(_) => {
            // Should not be encoding any files
            unreachable!();
        }
        ImageError::Parameter(e) => ImageError::Parameter(e),
        ImageError::Limits(e) => ImageError::Limits(e),
        ImageError::Unsupported(e) => {
            ImageError::Decoding(DecodingError::new(icns_format_hint(), e))
        }
        ImageError::IoError(e) => ImageError::IoError(e),
    }
}

/// A function type used to decode a square embedded image.
///
/// Arguments:
/// - `data: &[u8]`: complete data for the embedded image to decode
/// - `size: u32`: the expected width and height of the image. Will be `> 0` and `<= 1024`
/// - `buf: &mut [u8]`: array of bytes into which to write RGBA data. Will have size `4*size*size`
/// - `allocation_limit: u64`: a soft limit on how much memory to allocate while decoding
pub type SubformatDecodeFn = Box<dyn Fn(&[u8], u32, &mut [u8], u64) -> ImageResult<()>>;

/// A function of type [SubformatDecodeFn] that decodes the PNG image in `data` into `buf`.
///
/// This returns an error if data is invalid or the image's dimensions do no exactly
/// match (size, size).
fn decode_png(data: &[u8], size: u32, buf: &mut [u8], allocation_limit: u64) -> ImageResult<()> {
    let mut decoder = png::Decoder::new_with_limits(
        Cursor::new(data),
        png::Limits {
            bytes: allocation_limit.try_into().unwrap_or(usize::MAX),
        },
    );
    // Transform to produce La8 or Rgba8 output.
    // TODO: add flag GRAY_TO_RGB when `png` implements it
    decoder.set_transformations(png::Transformations::STRIP_16 | png::Transformations::ALPHA);
    let info = decoder.read_header_info().map_err(png_to_image_error)?;

    if info.width != size || info.height != size {
        return Err(DecoderError::BadPngSize(info.width, info.height, size).into());
    }

    let mut reader = decoder.read_info().map_err(png_to_image_error)?;
    let (color_type, bits) = reader.output_color_type();

    assert!(bits == png::BitDepth::Eight);
    match color_type {
        png::ColorType::GrayscaleAlpha => {
            // The space for this temporary vector was accounted for in `IcnsDecoder::set_limits`
            let mut tmp = vec![0u8; (size as usize) * (size as usize) * 2];
            reader.next_frame(&mut tmp).map_err(png_to_image_error)?;

            for (ga, rgba) in tmp.chunks_exact(2).zip(buf.chunks_exact_mut(4)) {
                rgba.copy_from_slice(&[ga[0], ga[0], ga[0], ga[1]]);
            }
        }
        png::ColorType::Rgba => {
            reader.next_frame(buf).map_err(png_to_image_error)?;
        }
        _ => unreachable!(),
    }

    reader.finish().map_err(png_to_image_error)?;

    Ok(())
}

/// A function of type [SubformatDecodeFn] that tries to decode the jpeg 2000 image
/// using the `image` crate's decoding hooks.
///
/// This will only work if a format hook for JPEG 2000 has been registered.
pub fn decode_jpeg2000_using_hook(
    data: &[u8],
    size: u32,
    buf: &mut [u8],
    allocation_limit: u64,
) -> ImageResult<()> {
    // The magic bytes have already been checked by IcnsDecoder, so it is unlikely
    // that a different format's decoder will be used be accident.
    // TODO:  explicitly set JPEG 2000 as the format, to be certain
    let mut reader = ImageReader::new(Cursor::new(data));
    reader = reader.with_guessed_format()?;
    let mut limits = Limits::no_limits();
    limits.max_alloc = Some(allocation_limit);
    reader.limits(limits);

    let image = reader.decode().map_err(jp2_to_image_error)?;
    if image.width() != size || image.height() != size {
        return Err(DecoderError::BadJp2Size(image.width(), image.height(), size).into());
    }

    buf.copy_from_slice(image.to_rgba8().as_flat_samples().samples);

    Ok(())
}

/// A function of type [SubformatDecodeFn] which just returns an error instead of trying
/// to decode the provided JPEG 2000 icon data.
pub fn unsupported_jpeg2000(
    _data: &[u8],
    _size: u32,
    _buf: &mut [u8],
    _allocation_limit: u64,
) -> ImageResult<()> {
    Err(ImageError::Unsupported(
        UnsupportedError::from_format_and_kind(
            icns_format_hint(),
            UnsupportedErrorKind::GenericFeature("Jpeg 2000 subimage".to_string()),
        ),
    ))
}

#[derive(Clone, Copy)]
struct IcnsEntry {
    code: IconType,
    stream_pos: u64,
    length: u32,
}

impl IcnsEntry {
    /// Return a lexicographically sortable score for how suitable it
    /// is as the "best" image in the file.
    ///
    /// This requires that the entry contains non-mask content (does not have encoding Mask8).
    fn score(&self) -> (u32, u8, u64) {
        let bit_depth = match self.code.encoding() {
            Encoding::Mask8 => {
                panic!("Entry with color data required, not Mask8");
            }
            Encoding::Mono => 1,
            Encoding::MonoA => 2,
            // Paletted entries have an associated 1 bit mask
            Encoding::Palette4 => 5,
            Encoding::Palette8 => 9,
            // RLE24 entries have an associated 8 bit mask
            Encoding::RLE24 => 32,
            Encoding::JP2PNG => 32,
        };
        (
            self.code.pixel_width(),
            bit_depth,
            u64::MAX - self.stream_pos,
        )
    }
}

/// ICNS decoder
pub struct IcnsDecoder<R> {
    reader: R,

    main: IcnsEntry,
    // If a mask entry applies to the main image, its details will be indicated here
    mask: Option<IcnsEntry>,

    limits: Limits,
    jp2: SubformatDecodeFn,
}

/// Move forward in the reader by `skip` bytes.
///
/// Seeking forward a type implementing Read + Seek can be surprisingly inefficient
/// for typical implementations; for example, BufReader will by default discard and
/// reload its entire buffer every time it seeks forward, even if the seek is very short.
///
/// To avoid reading many more bytes than necessary, this function reads if the skip
/// amount is small, and seeks if it is large.
fn seek_or_read_forward<R>(reader: &mut R, skip: u32) -> Result<(), std::io::Error>
where
    R: Read + Seek,
{
    const READ_LEN: usize = 512;
    if skip as usize <= READ_LEN {
        let mut tmp = [0u8; READ_LEN];
        reader.read_exact(&mut tmp[..skip as usize])?;
    } else {
        reader.seek(SeekFrom::Current(skip as i64))?;
    }
    Ok(())
}

/// Read the data from the reader for the stream position range `start..start + len`
fn read_vec_at<R>(reader: &mut R, start: u64, len: u32) -> Result<Vec<u8>, std::io::Error>
where
    R: Read + Seek,
{
    assert!(start.checked_add(len as u64).is_some());
    reader.seek(SeekFrom::Start(start))?;

    let mut data = vec![0; len.try_into().unwrap()];
    reader.read_exact(&mut data)?;
    Ok(data)
}

impl<R> IcnsDecoder<R>
where
    R: Read + Seek,
{
    /// Create a new `IcnsDecoder` and seek around the input file to locate
    /// the "best" image. ("best" here means largest, breaking ties to prefer
    /// higher total bit depth; if still tied the earliest image is chosen.)
    ///
    /// The resulting decoder does not support decoding Jpeg2000 image entries.
    /// Use [IcnsDecoder::new_with_decode_funcs] if you'd like to supply your
    /// own function for that.
    pub fn new(reader: R) -> Result<IcnsDecoder<R>, ImageError> {
        Self::new_with_decode_func(reader, Box::new(unsupported_jpeg2000))
    }

    /// Create a new `IcnsDecoder` and seek around the input file to locate
    /// the "best" image. ("best" here means largest, breaking ties to prefer
    /// higher total bit depth; if still tied the earliest image is chosen.)
    ///
    /// The ICNS format can nest PNG and JP2 images; this function accepts a
    /// function that can be used to decode the JP2 images. See for example
    /// [unsupported_jpeg2000], and [decode_jpeg2000_using_hook].
    pub fn new_with_decode_func(
        mut reader: R,
        jp2: SubformatDecodeFn,
    ) -> Result<IcnsDecoder<R>, ImageError> {
        let mut header = [0u8; 8];
        reader.read_exact(&mut header)?;
        let (magic, file_len_field) = header.split_at(4);
        if magic != b"icns" {
            return Err(DecoderError::NotICNS.into());
        }
        let file_length = u32::from_be_bytes(file_len_field.try_into().unwrap());
        let Some(remaining_len) = file_length.checked_sub(8) else {
            return Err(DecoderError::BadEntryLength.into());
        };

        // Record all decodable entries. Because only the first entry of each known
        // icon type is recorded, the map's size is bounded.
        let mut first_entries: HashMap<IconType, IcnsEntry> = HashMap::new();

        let base_pos = reader.stream_position()?;
        let Some(end) = base_pos.checked_add(u64::from(remaining_len)) else {
            return Err(DecoderError::ImageEndAfterEndOfStream.into());
        };

        // Loop over all entries of the file, ignoring unrecognized elements.
        let mut cur_pos = base_pos;
        while cur_pos < end {
            let image_start_pos = cur_pos;
            if cur_pos > end.saturating_sub(8) {
                return Err(DecoderError::IncompleteEntry.into());
            }

            let mut entry = [0u8; 8];
            reader.read_exact(&mut entry)?;
            let (ostype_field, entry_len_field) = entry.split_at(4);
            let ostype = OSType(ostype_field.try_into().unwrap());
            let entry_len = u32::from_be_bytes(entry_len_field.try_into().unwrap());

            let Some(data_len) = entry_len.checked_sub(8) else {
                return Err(DecoderError::BadEntryLength.into());
            };
            if cur_pos > end.saturating_sub(entry_len as u64) {
                return Err(DecoderError::IncompleteEntry.into());
            }
            if cur_pos < end {
                seek_or_read_forward(&mut reader, data_len)?;
            }
            cur_pos += entry_len as u64;

            // Identify the entry
            let Some(code) = IconType::from_ostype(ostype) else {
                // Silently ignore unrecognized fields; ICNS files may contain more data
                continue;
            };

            let entry = IcnsEntry {
                code,
                stream_pos: image_start_pos + 8,
                length: data_len,
            };
            if first_entries.insert(code, entry).is_some() {
                return Err(DecoderError::DuplicateEntry(code).into());
            }
        }

        let main_entry = first_entries
            .values()
            .filter(|entry| entry.code.encoding() != Encoding::Mask8)
            .max_by_key(|entry| entry.score());

        let Some(main) = main_entry.copied() else {
            return Err(DecoderError::NoImageFound.into());
        };
        let mut mask = None;
        if let Some(mtype) = main.code.mask_type() {
            mask = first_entries.get(&mtype).copied();
            if mask.is_none() {
                return Err(DecoderError::MissingMask(main.code).into());
            }
        }
        Ok(IcnsDecoder {
            reader,
            main,
            mask,
            limits: Limits::no_limits(),
            jp2,
        })
    }
}

impl<R: Read + Seek> ImageDecoder for IcnsDecoder<R> {
    fn dimensions(&self) -> (u32, u32) {
        (self.main.code.pixel_width(), self.main.code.pixel_height())
    }

    fn color_type(&self) -> ColorType {
        match self.main.code.encoding() {
            Encoding::Mask8 => unreachable!(),
            Encoding::Mono => ColorType::L8,
            Encoding::MonoA => ColorType::La8,
            _ => ColorType::Rgba8,
        }
    }

    fn set_limits(&mut self, mut limits: Limits) -> ImageResult<()> {
        limits.check_support(&LimitSupport::default())?;
        let (width, height) = self.dimensions();
        limits.check_dimensions(width, height)?;

        let icon_size = self.main.code.pixel_width();

        let main_data = self.main.length;
        let mask_data = self.mask.map(|x| x.length).unwrap_or_default();

        // Decoding non PNG/JP2 icons using `icns` can create temporary images using
        // a maximum of about 4 + 4 bytes per pixel (up to 4 for the initial decoding
        // result, plus up to 4 to store a conversion to RGBA.) This may change if the
        // dependency is updated.
        //
        // Decoding PNG icon entries may use an additional 2 bytes per pixel (to handle
        // grayscale images), which is also < 8.
        //
        // Tight memory bounds aren't critical here, because ICNS entries are at most
        // 1024x1024 pixels.
        assert!(icon_size <= 1024);
        let icon_decode_space = icon_size * icon_size * 8;

        let space_req = u64::from(icon_decode_space) + u64::from(main_data) + u64::from(mask_data);
        limits.reserve(space_req)?;

        self.limits = limits;

        Ok(())
    }

    fn read_image(mut self, buf: &mut [u8]) -> ImageResult<()> {
        assert!(self.total_bytes() == buf.len().try_into().unwrap());

        let main_data = read_vec_at(&mut self.reader, self.main.stream_pos, self.main.length)?;

        const PNG_SIGNATURE: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
        const JP2_SIGNATURE: &[u8] = &[
            0x00, 0x00, 0x00, 0x0C, 0x6A, 0x50, 0x20, 0x20, 0x0D, 0x0A, 0x87, 0x0A,
        ];

        if self.main.code.encoding() == Encoding::JP2PNG {
            // Handle JP2 or PNG images _directly_ in this implementation, in order
            // to implement memory limits and make it easier to keep these complicated
            // format decoders up to date.
            if main_data.starts_with(PNG_SIGNATURE) {
                decode_png(
                    &main_data,
                    self.main.code.pixel_width(),
                    buf,
                    self.limits.max_alloc.unwrap_or(u64::MAX),
                )?;
            } else if main_data.starts_with(JP2_SIGNATURE) {
                (self.jp2)(
                    &main_data,
                    self.main.code.pixel_width(),
                    buf,
                    self.limits.max_alloc.unwrap_or(u64::MAX),
                )?;
            } else {
                return Err(DecoderError::NotPNGorJP2(self.main.code).into());
            }
        } else {
            let main = IconElement::new(self.main.code.ostype(), main_data);

            let img = if let Some(mask_entry) = &self.mask {
                let mask_data =
                    read_vec_at(&mut self.reader, mask_entry.stream_pos, mask_entry.length)?;
                let mask = IconElement::new(mask_entry.code.ostype(), mask_data);

                main.decode_image_with_mask(&mask)?
            } else {
                assert!(self.main.code.mask_type().is_none());
                main.decode_image()?
            };
            assert!((img.width(), img.height()) == self.dimensions());
            assert!(img.pixel_format() != PixelFormat::Alpha);

            match (img.pixel_format(), self.color_type()) {
                (PixelFormat::Gray, ColorType::L8) => {
                    buf.copy_from_slice(img.data());
                }
                (PixelFormat::GrayAlpha, ColorType::La8) => {
                    buf.copy_from_slice(img.data());
                }
                (PixelFormat::RGBA, ColorType::Rgba8)
                | (PixelFormat::RGB, ColorType::Rgba8)
                | (PixelFormat::Gray, ColorType::Rgba8)
                | (PixelFormat::GrayAlpha, ColorType::Rgba8) => {
                    let converted = img.convert_to(PixelFormat::RGBA);
                    buf.copy_from_slice(converted.data());
                }

                // This format combination would be an error in the `icns` crate
                _ => unreachable!(
                    "icns crate produced {:?}, not compatible with {:?}",
                    img.pixel_format(),
                    self.color_type()
                ),
            };
        }

        Ok(())
    }

    fn read_image_boxed(self: Box<Self>, buf: &mut [u8]) -> ImageResult<()> {
        (*self).read_image(buf)
    }
}

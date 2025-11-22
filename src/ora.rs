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
use ouroboros::self_referencing;
use std::io::{self, BufReader, Read, Seek};
use std::marker::PhantomData;
use zip::read::{ZipArchive, ZipFile};

pub struct OpenRasterDecoder<'a, R>
where
    R: Read + Seek + 'a,
{
    mergedimg_decoder: PngDecoder<BufReader<SeekableArchiveFile<'a, R>>>,
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

#[self_referencing]
struct SeekableArchiveCore<'a, R: Read + Seek + 'a> {
    archive: ZipArchive<R>,
    #[covariant]
    #[borrows(mut archive)]
    file: ZipFile<'this, R>,
    lifetime_helper: PhantomData<&'a R>,
}

/// The zip crate does not provide a seekable reader that works on compressed
/// entries, while png::Decoder requires the Seek bound (but does not currently
/// use it). This structure implements Seek by reopening and reading the zip
/// archive entry whenever it seeks backwards.
struct SeekableArchiveFile<'a, R: Read + Seek + 'a> {
    core: Option<SeekableArchiveCore<'a, R>>,
    file_index: usize,
    position: u64,
    file_size: u64,
}

impl<'a, R: Read + Seek + 'a> SeekableArchiveFile<'a, R> {
    fn new(
        archive: ZipArchive<R>,
        file_index: usize,
    ) -> Result<SeekableArchiveFile<'a, R>, io::Error> {
        let core = SeekableArchiveCore::try_new(archive, |x| x.by_index(file_index), PhantomData)
            .map_err(|x| io::Error::other(format!("failed to open: {:?}", x)))?;
        let file_size = core.with_file(|file| file.size());
        Ok(SeekableArchiveFile {
            core: Some(core),
            file_index,
            position: 0,
            file_size,
        })
    }
}

impl<R: Read + Seek> Read for SeekableArchiveFile<'_, R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let res = self
            .core
            .as_mut()
            .unwrap()
            .with_file_mut(|file| file.read(buf));
        let nread = res?;
        self.position
            .checked_add(nread as u64)
            .ok_or_else(|| io::Error::other("seek position overflow"))?;
        Ok(nread)
    }
}

impl<R: Read + Seek> Seek for SeekableArchiveFile<'_, R> {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        let target_pos = match pos {
            io::SeekFrom::Start(offset) => offset,
            io::SeekFrom::End(offset) => self
                .file_size
                .checked_add_signed(offset)
                .ok_or_else(|| io::Error::other("seek position over or underflow"))?,
            io::SeekFrom::Current(offset) => self
                .position
                .checked_add_signed(offset)
                .ok_or_else(|| io::Error::other("seek position over or underflow"))?,
        };

        if target_pos < self.position {
            let core = self.core.take();
            let archive = core.unwrap().into_heads().archive;

            self.core = Some(
                SeekableArchiveCore::try_new(archive, |x| x.by_index(self.file_index), PhantomData)
                    .map_err(|x| io::Error::other(format!("failed to reopen: {:?}", x)))?,
            );
        }
        while self.position < target_pos {
            const TMP_LEN: usize = 1024;
            let mut tmp = [0_u8; TMP_LEN];
            let cur_pos = self.position;
            let nr = self
                .read(&mut tmp[..std::cmp::min(TMP_LEN as u64, target_pos - cur_pos) as usize])?;
            if nr == 0 {
                return Err(io::Error::other("unexpected eof when seeking"));
            }
            self.position += nr as u64;
        }

        Ok(0)
    }
}

impl<'a, R> OpenRasterDecoder<'a, R>
where
    R: Read + Seek + 'a,
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
    pub fn with_limits(r: R, limits: Limits) -> Result<OpenRasterDecoder<'a, R>, ImageError> {
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

        let mergedimage_index = archive.index_for_name("mergedimage.png").ok_or_else(|| {
            ImageError::Decoding(DecodingError::new(
                openraster_format_hint(),
                "OpenRaster image missing mergedimage.png entry",
            ))
        })?;

        let file = SeekableArchiveFile::new(archive, mergedimage_index)?;
        let decoder =
            PngDecoder::with_limits(BufReader::new(file), limits).map_err(set_ora_image_type)?;

        Ok(OpenRasterDecoder {
            mergedimg_decoder: decoder,
        })
    }
}

impl<'a, R: Read + Seek + 'a> ImageDecoder for OpenRasterDecoder<'a, R> {
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

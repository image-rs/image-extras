use std::path::{Path, PathBuf};

use image::{ColorType, DynamicImage, ImageResult};
use walkdir::WalkDir;

fn is_bless() -> bool {
    std::env::var("BLESS").is_ok()
}

/// Test decoding of all test images in `tests/decode/` against reference images
/// (either PNG or TIFF).
///
/// ## Bless mode
///
/// If the `BLESS` environment variable is set, the test will update the reference images
/// to match the newly decoded images. This is useful when adding new test images,
/// updating existing ones, or seeing the output of incorrectly decoded images.
///
/// ```sh
/// BLESS=1 cargo test
/// ```
/// ```powershell
/// $env:BLESS=1; cargo test; $env:BLESS=$null
/// ```
#[test]
fn test_decoding() {
    image_extras::register();

    let bless = is_bless();
    let mut errors = vec![];

    for entry in WalkDir::new("tests/decode") {
        let entry = entry.unwrap();
        if !entry.file_type().is_file()
            || entry.path().extension().unwrap() == "png"
            || entry.path().extension().unwrap() == "tiff"
        {
            continue;
        }

        let mut add_error = |e: &str| {
            errors.push(format!("{}: {}", entry.path().display(), e));
        };

        let img = match image::open(entry.path()) {
            Ok(i) => i,
            Err(e) => {
                add_error(&format!("Cannot decode image: {e}"));
                continue;
            }
        };

        let ref_format = match img.color() {
            ColorType::Rgb32F | ColorType::Rgba32F => "tiff",
            _ => "png",
        };
        let ref_path = &entry.path().with_extension(ref_format);

        let save_reference = || {
            if bless {
                _ = img.save(ref_path); // save and ignore errors
            }
        };

        if !ref_path.exists() {
            add_error("No reference PNG file found");
            save_reference();
            continue;
        }

        let reference = image::open(ref_path).unwrap();

        if bless && img != reference {
            add_error("Does not match reference");
            save_reference();
            continue;
        }

        assert_eq!(
            img,
            reference,
            "Image {} does not match reference",
            entry.path().display()
        );
    }

    if !errors.is_empty() {
        panic!("Decoding errors:\n{}", errors.join("\n"));
    }
}

#[test]
fn test_encoding() {
    image_extras::register();

    let test_image = &NamedImage::from_samples("test-image.png");

    encode(
        "otb",
        "otb",
        |w, img| img.write_with_encoder(image_extras::otb::OtbEncoder::new(w)),
        &[&test_image.with_color_type(ColorType::L8)],
    );
    encode(
        "otb threshold",
        "otb",
        |w, img| img.write_with_encoder(image_extras::otb::OtbEncoder::new(w).with_threshold(255)),
        &[&test_image.with_color_type(ColorType::L8)],
    );
    encode(
        "wbmp",
        "wbmp",
        |w, img| img.write_with_encoder(image_extras::wbmp::WbmpEncoder::new(w)),
        &[
            &test_image.with_color_type(ColorType::L8),
            &test_image.with_color_type(ColorType::Rgba8),
        ],
    );
    encode(
        "wbmp threshold",
        "wbmp",
        |w, img| {
            img.write_with_encoder(image_extras::wbmp::WbmpEncoder::new(w).with_threshold(255))
        },
        &[
            &test_image.with_color_type(ColorType::L8),
            &test_image.with_color_type(ColorType::Rgba8),
        ],
    );

    fn encode(
        dir: &str,
        ext: &str,
        write: impl Fn(&mut Vec<u8>, &DynamicImage) -> ImageResult<()>,
        images: &[&NamedImage],
    ) {
        let bless = is_bless();
        let out_dir = PathBuf::from("tests/encode").join(dir);
        std::fs::create_dir_all(&out_dir).unwrap();

        for &image in images {
            let mut buffer = Vec::new();
            let result = write(&mut buffer, &image.image);
            if let Err(e) = result {
                panic!("Failed to encode image {} for {dir}: {e}", image.name);
            }

            let out_path = out_dir.join(format!("{}.{}", image.name, ext));
            if bless || !out_path.exists() {
                std::fs::write(&out_path, &buffer).unwrap();
            } else {
                let reference = std::fs::read(&out_path).unwrap();
                assert_eq!(
                    buffer,
                    reference,
                    "Encoded image {} does not match reference at {}",
                    image.name,
                    out_path.display()
                );
            }

            let reopen = match image::open(&out_path) {
                Ok(i) => i,
                Err(e) => {
                    panic!(
                        "Cannot reopen encoded image {} at {}: {e}",
                        image.name,
                        out_path.display()
                    );
                }
            };
            reopen.save(out_path.with_added_extension("png")).unwrap();
        }
    }

    #[derive(Clone)]
    struct NamedImage {
        name: String,
        image: DynamicImage,
    }
    impl NamedImage {
        fn from_samples(name: &str) -> Self {
            Self::from_file(&PathBuf::from("tests/samples").join(name))
        }
        fn from_file(path: &Path) -> Self {
            let name = path.file_stem().unwrap().to_string_lossy().to_string();
            let image = image::open(path).unwrap();
            Self { name, image }
        }

        fn with_color_type(&self, color_type: ColorType) -> Self {
            let image: DynamicImage = match color_type {
                ColorType::L8 => self.image.to_luma8().into(),
                ColorType::La8 => self.image.to_luma_alpha8().into(),
                ColorType::Rgb8 => self.image.to_rgb8().into(),
                ColorType::Rgba8 => self.image.to_rgba8().into(),
                ColorType::L16 => self.image.to_luma16().into(),
                ColorType::La16 => self.image.to_luma_alpha16().into(),
                ColorType::Rgb16 => self.image.to_rgb16().into(),
                ColorType::Rgba16 => self.image.to_rgba16().into(),
                _ => panic!("Unsupported color type {:?}", color_type),
            };

            Self {
                name: format!("{}_{:?}", self.name, color_type),
                image,
            }
        }
    }
}

use std::path::PathBuf;

use image::ColorType;
use tempfile::tempdir;
use walkdir::WalkDir;

fn iter_decoding_images() -> impl Iterator<Item = PathBuf> {
    WalkDir::new("tests/images")
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().is_file()
                && entry.path().extension().unwrap() != "png"
                && entry.path().extension().unwrap() != "tiff"
        })
        .map(|entry| entry.path().to_owned())
}

/// Test decoding of all test images in `tests/images/` against reference images
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

    let bless = std::env::var("BLESS").is_ok();
    let mut errors = vec![];

    for img_path in iter_decoding_images() {
        let img_path = img_path.as_path();

        let mut add_error = |e: &str| {
            errors.push(format!("{}: {}", img_path.display(), e));
        };

        let img = match image::open(img_path) {
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
        let ref_path = &img_path.with_extension(ref_format);

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
            img_path.display()
        );
    }

    if !errors.is_empty() {
        panic!("Decoding errors:\n{}", errors.join("\n"));
    }
}

/// This test takes valid images from `tests/images/` and applies various
/// modifications to them (truncation, extension with garbage data, byte mutations)
/// and ensures that decoding them does not panic.
#[test]
fn test_decoding_variations() {
    image_extras::register();

    let tmp_dir = tempdir().unwrap();

    for img_path in iter_decoding_images() {
        let img_path = img_path.as_path();
        let ext = img_path.extension().unwrap().to_string_lossy().to_string();

        let bytes = std::fs::read(img_path).unwrap();

        // test truncations
        for i in (0..1000.min(bytes.len())).step_by(37) {
            let shorter_path = tmp_dir.path().join(format!("truncation.{ext}"));
            std::fs::write(&shorter_path, &bytes[..i]).unwrap();
            _ = image::open(shorter_path); // just check it doesn't panic
        }

        // test extension
        let mut longer = bytes.clone();
        longer.extend_from_slice(b"I am garbage data at the end of the file!");
        let longer_path = tmp_dir.path().join(format!("extension.{ext}"));
        std::fs::write(&longer_path, &longer).unwrap();
        _ = image::open(longer_path); // just check it doesn't panic

        // test mutations
        let mut mutations_buffer = bytes.clone().into_boxed_slice();
        for i in 0..16.min(mutations_buffer.len()) {
            let mask = 0b0101_0101;
            mutations_buffer[i] ^= mask;

            let shorter_path = tmp_dir.path().join(format!("mutations.{ext}"));
            std::fs::write(&shorter_path, &bytes[..i]).unwrap();
            _ = image::open(shorter_path); // just check it doesn't panic

            // undo mutation
            mutations_buffer[i] ^= mask;
        }
    }

    tmp_dir.close().unwrap();
}

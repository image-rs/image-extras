use image::ColorType;
use walkdir::WalkDir;

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

    for entry in WalkDir::new("tests/images") {
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

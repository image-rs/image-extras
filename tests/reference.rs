use image::ColorType;
use walkdir::WalkDir;

/// Test decoding of all images in `tests/images/` against reference PNG files.
///
/// If a reference PNG file for an image does not exist or does not match,
/// it will be created/overwritten with the newly decoded image.
///
/// To add new test images, simply place them under `tests/images/{my format}/`
/// and run `cargo test`.
///
/// Note: Images containing f32 data will be converted to u16, because PNG
/// does not support floating-point data.
#[test]
fn test_decoding() {
    image_extras::register();

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
        let ref_path = entry.path().with_extension(ref_format);

        let save_reference = || {
            _ = img.save(ref_path); // save and ignore errors
        };

        if !ref_path.exists() {
            add_error("No reference PNG file found");
            save_reference();
            continue;
        }

        let reference = image::open(&png_path).unwrap();

        if img != reference {
            add_error("Does not match reference");
            save_reference();
        }
    }

    if !errors.is_empty() {
        panic!("Decoding errors:\n{}", errors.join("\n"));
    }
}

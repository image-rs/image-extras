use image::{ColorType, DynamicImage};
use walkdir::WalkDir;

#[test]
fn test_decoding() {
    image_extras::register();

    for entry in WalkDir::new("tests/images") {
        let entry = entry.unwrap();
        if !entry.file_type().is_file() || entry.path().extension().unwrap() == "png" {
            continue;
        }

        let img = to_png_compatible_color_type(image::open(entry.path()).unwrap());
        let reference = image::open(entry.path().with_extension("png")).unwrap();

        assert_eq!(
            img,
            reference,
            "Image {} does not match reference",
            entry.path().display()
        );
    }
}

/// PNG doesn't support 32-bit float color types, so convert them to 16-bit.
fn to_png_compatible_color_type(image: DynamicImage) -> DynamicImage {
    match image.color() {
        ColorType::Rgb32F => image.to_rgb16().into(),
        ColorType::Rgba32F => image.to_rgba16().into(),
        _ => image,
    }
}

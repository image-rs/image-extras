use walkdir::WalkDir;

#[test]
fn test_decoding() {
    image_extras::register();

    for entry in WalkDir::new("tests/images") {
        let entry = entry.unwrap();
        if !entry.file_type().is_file() || entry.path().extension().unwrap() == "png" {
            continue;
        }

        let img = image::open(entry.path()).unwrap();
        let reference = image::open(entry.path().with_extension("png")).unwrap();

        assert_eq!(
            img,
            reference,
            "Image {} does not match reference",
            entry.path().display()
        );
    }
}

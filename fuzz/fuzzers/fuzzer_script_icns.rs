#![no_main]
use libfuzzer_sys::fuzz_target;

use image::ImageDecoder;
use std::io::Cursor;

fuzz_target!(|data: &[u8]| {
    let reader = Cursor::new(data);
    let Ok(mut decoder) = image_extras::icns::IcnsDecoder::new_with_decode_func(
        reader,
        Box::new(image_extras::icns::decode_jpeg2000_using_hook),
    ) else {
        return;
    };
    let mut limits = image::Limits::default();
    limits.max_alloc = Some(32 * 1024 * 1024); // 32 MiB
    if limits.reserve(decoder.total_bytes()).is_err() {
        return;
    }
    if decoder.set_limits(limits).is_err() {
        return;
    }
    let _ = std::hint::black_box(image::DynamicImage::from_decoder(decoder));
});

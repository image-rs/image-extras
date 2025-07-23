#![no_main]
#[macro_use]
extern crate libfuzzer_sys;

use image::ImageDecoder;
use std::io::Cursor;

fuzz_target!(|data: &[u8]| {
    let reader = Cursor::new(data);
    let Ok(mut decoder) = image_extras::sgi::SgiDecoder::new(reader) else {
        return;
    };
    let mut limits = image::Limits::default();
    limits.max_alloc = Some(1024 * 1024); // 1 MiB
    if limits.reserve(decoder.total_bytes()).is_err() {
        return;
    }
    if decoder.set_limits(limits).is_err() {
        return;
    }
    let _ = std::hint::black_box(image::DynamicImage::from_decoder(decoder));
});

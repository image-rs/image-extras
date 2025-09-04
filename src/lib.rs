//! This crate provides additional formats for the image crate.
//!
//! The enabled formats are controlled via Cargo features:
//! ```toml
//! [dependencies]
//! image_extras = { version = "0.1", features = ["pcx"] }
//! ```
//!
//! And you must also call the `register` function at program startup:
//!
//!  ```rust,no_run
//! image_extras::register();
//!
//! // Now you can use the image crate as normal
//! let img = image::open("path/to/image.pcx").unwrap();
//! ```

#[cfg(feature = "pcx")]
pub mod pcx;

#[cfg(feature = "sgi")]
pub mod sgi;

/// Register all enabled extra formats with the image crate.
pub fn register() {
    let just_registered_pcx = image::hooks::register_decoding_hook(
        "pcx".into(),
        Box::new(|r| Ok(Box::new(pcx::PCXDecoder::new(r)?))),
    );
    if just_registered_pcx {
        image::hooks::register_format_detection_hook("pcx".into(), &[0x0a, 0x0], Some(b"\xFF\xF8"));
    }

    // SGI RGB images generally show up with a .rgb ending (whether or not they
    // have 3 channels), and sometimes .bw (when grayscale) and .rgba. The
    // extensions .sgi and .iris, while unambiguous, do not seem to have been
    // used much. The extension .rgb is also used for a variety of other files,
    // including bare image data, so to be sure it would be best to check both
    // extension and leading bytes
    let hook: for<'a> fn(
        image::hooks::GenericReader<'a>,
    ) -> image::ImageResult<Box<dyn image::ImageDecoder + 'a>> =
        |r| Ok(Box::new(sgi::SgiDecoder::new(r)?));
    image::hooks::register_decoding_hook("bw".into(), Box::new(hook));
    image::hooks::register_decoding_hook("rgb".into(), Box::new(hook));
    image::hooks::register_decoding_hook("rgba".into(), Box::new(hook));
    image::hooks::register_decoding_hook("iris".into(), Box::new(hook));
    let just_registered_sgi = image::hooks::register_decoding_hook("sgi".into(), Box::new(hook));
    if just_registered_sgi {
        // The main signature bytes are technically just 01 da, but this is short
        // and the following storage and bpc fields are constrained well enough to
        // efficiently match them as well
        image::hooks::register_format_detection_hook("sgi".into(), b"\x01\xda\x00\x01", None);
        image::hooks::register_format_detection_hook("sgi".into(), b"\x01\xda\x01\x01", None);
        image::hooks::register_format_detection_hook("sgi".into(), b"\x01\xda\x00\x02", None);
        image::hooks::register_format_detection_hook("sgi".into(), b"\x01\xda\x01\x02", None);
    }
}

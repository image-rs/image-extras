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

/// Register all enabled extra formats with the image crate.
pub fn register() {
    image::hooks::register_decoding_hook(
        "pcx".into(),
        Box::new(|r| Ok(Box::new(pcx::PCXDecoder::new(r)?))),
    );
    image::hooks::register_format_detection_hook("pcx".into(), &[0x0a, 0x0], Some(b"\xFF\xF8"));
}

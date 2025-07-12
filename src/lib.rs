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

#[cfg(feature = "xbm")]
pub mod xbm;

#[cfg(feature = "xpm")]
pub mod xpm;

/// Register all enabled extra formats with the image crate.
pub fn register() {
    let just_registered_pcx = image::hooks::register_decoding_hook(
        "pcx".into(),
        Box::new(|r| Ok(Box::new(pcx::PCXDecoder::new(r)?))),
    );
    if just_registered_pcx {
        image::hooks::register_format_detection_hook("pcx".into(), &[0x0a, 0x0], Some(b"\xFF\xF8"));
    }

    // XBM images are often valid C code and have no simple and reliably distinguishing file signature
    image::hooks::register_decoding_hook(
        "xbm".into(),
        Box::new(|r| Ok(Box::new(xbm::XbmDecoder::new(r)?))),
    );
    image::hooks::register_decoding_hook(
        "bm".into(),
        Box::new(|r| Ok(Box::new(xbm::XbmDecoder::new(r)?))),
    );

    let just_registered_xpm = image::hooks::register_decoding_hook(
        "xpm".into(),
        Box::new(|r| Ok(Box::new(xpm::XpmDecoder::new(r)?))),
    );
    if just_registered_xpm {
        image::hooks::register_format_detection_hook("xpm".into(), b"/* XPM */", None);
    }
}

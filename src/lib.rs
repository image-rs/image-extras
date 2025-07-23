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

#![forbid(unsafe_code)]

#[cfg(feature = "ora")]
pub mod ora;

#[cfg(feature = "otb")]
pub mod otb;

#[cfg(feature = "pcx")]
pub mod pcx;

#[cfg(feature = "sgi")]
pub mod sgi;

#[cfg(feature = "wbmp")]
pub mod wbmp;

#[cfg(feature = "xbm")]
pub mod xbm;

#[cfg(feature = "xpm")]
pub mod xpm;

#[allow(unused_imports)]
use image::hooks::{register_decoding_hook, register_format_detection_hook};

static REGISTER: std::sync::Once = std::sync::Once::new();

/// Register all enabled extra formats with the image crate.
pub fn register() {
    REGISTER.call_once(|| {
        // OpenRaster images are ZIP files and have no simple signature to distinguish them
        // from ZIP files containing other content
        #[cfg(feature = "ora")]
        register_decoding_hook(
            "ora".into(),
            Box::new(|r| {
                Ok(Box::new(ora::OpenRasterDecoder::with_limits(
                    r,
                    image::Limits::no_limits(),
                )?))
            }),
        );

        #[cfg(feature = "otb")]
        image::hooks::register_decoding_hook(
            "otb".into(),
            Box::new(|r| Ok(Box::new(otb::OtbDecoder::new(r)?))),
        );

        #[cfg(feature = "pcx")]
        if register_decoding_hook(
            "pcx".into(),
            Box::new(|r| Ok(Box::new(pcx::PCXDecoder::new(r)?))),
        ) {
            register_format_detection_hook("pcx".into(), &[0x0a, 0x0], Some(b"\xFF\xF8"));
        }

        // SGI RGB images generally show up with a .rgb ending (whether or not they
        // have 3 channels), and sometimes .bw (when grayscale) and .rgba. The
        // extensions .sgi and .iris, while unambiguous, do not seem to have been
        // used much. The extension .rgb is also used for a variety of other files,
        // including bare image data, so to be sure it would be best to check both
        // extension and leading bytes
        #[cfg(feature = "sgi")]
        {
            let hook: for<'a> fn(
                image::hooks::GenericReader<'a>,
            )
                -> image::ImageResult<Box<dyn image::ImageDecoder + 'a>> =
                |r| Ok(Box::new(sgi::SgiDecoder::new(r)?));
            image::hooks::register_decoding_hook("bw".into(), Box::new(hook));
            image::hooks::register_decoding_hook("rgb".into(), Box::new(hook));
            image::hooks::register_decoding_hook("rgba".into(), Box::new(hook));
            image::hooks::register_decoding_hook("iris".into(), Box::new(hook));
            if image::hooks::register_decoding_hook("sgi".into(), Box::new(hook)) {
                // The main signature bytes are technically just 01 da, but this is short
                // and the following storage and bpc fields are constrained well enough to
                // efficiently match them as well
                image::hooks::register_format_detection_hook(
                    "sgi".into(),
                    b"\x01\xda\x00\x01",
                    None,
                );
                image::hooks::register_format_detection_hook(
                    "sgi".into(),
                    b"\x01\xda\x01\x01",
                    None,
                );
                image::hooks::register_format_detection_hook(
                    "sgi".into(),
                    b"\x01\xda\x00\x02",
                    None,
                );
                image::hooks::register_format_detection_hook(
                    "sgi".into(),
                    b"\x01\xda\x01\x02",
                    None,
                );
            }
        }

        #[cfg(feature = "wbmp")]
        image::hooks::register_decoding_hook(
            "wbmp".into(),
            Box::new(|r| Ok(Box::new(wbmp::WbmpDecoder::new(r)?))),
        );

        #[cfg(feature = "xbm")]
        {
            register_decoding_hook(
                "xbm".into(),
                Box::new(|r| Ok(Box::new(xbm::XbmDecoder::new(r)?))),
            );
            register_decoding_hook(
                "bm".into(),
                Box::new(|r| Ok(Box::new(xbm::XbmDecoder::new(r)?))),
            );
        }

        #[cfg(feature = "xpm")]
        if register_decoding_hook(
            "xpm".into(),
            Box::new(|r| Ok(Box::new(xpm::XpmDecoder::new(r)?))),
        ) {
            register_format_detection_hook("xpm".into(), b"/* XPM */", None);
        }
    });
}

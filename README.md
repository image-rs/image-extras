# image-extras

Support for additional image formats beyond those provided by the
[`image`](https://crates.io/crates/image) crate.

## Usage

Call the `register` function at program startup:

```rust
fn main() {
    image_extras::register();

    // Now you can use the image crate as normal
    let img = image::open("path/to/image.pcx").unwrap();
}
```

By default, all supported formats are enabled. For finer control, enable
individual formats via Cargo features:

```toml
[dependencies]
image_extras = { version = "0.1", features = ["pcx"], default-features = false }
```

## Supported Formats

| Feature | Format
| ------- | ------
| `icns`  | ICNS [\[desc\]](https://en.wikipedia.org/wiki/Apple_Icon_Image_format)
| `ora`   | OpenRaster [\[spec\]](https://www.openraster.org/)
| `otb`   | OTA Bitmap (Over The Air Bitmap) [\[desc\]](https://en.wikipedia.org/wiki/OTA_bitmap)
| `pcx`   | PCX (ZSoft Paintbrush bitmap/PiCture eXchange) [\[desc\]](https://en.wikipedia.org/wiki/PCX#PCX_file_format)
| `sgi`   | SGI (Silicon Graphics Image) [\[spec\]](https://web.archive.org/web/20010413154909/https://reality.sgi.com/grafica/sgiimage.html)
| `wbmp`  | Wireless Bitmap [\[spec\]](https://www.wapforum.org/what/technical/SPEC-WAESpec-19990524.pdf)
| `xbm`   | X BitMap [\[spec\]](https://www.x.org/releases/X11R7.7/doc/libX11/libX11/libX11.html#Manipulating_Bitmaps)
| `xpm`   | X PixMap [\[spec\]](https://www.x.org/docs/XPM/xpm.pdf)

By default, `image-extras` enables support for all included formats. This is
convenient for prototyping, but for other uses you are encouraged to evaluate
the individual implementations and enable only the ones that meet your
quality/robustness requirements.

### New Formats

We welcome PRs to add support for additional image formats.

#### Required criteria

- [ ] Must be one of the raster image formats [recognized by ImageMagick](https://imagemagick.org/script/formats.php).
- [ ] No patent or licensing restrictions.
- [ ] Specification or sufficiently detailed file format description freely available online.
- [ ] Must include multiple test images, and their source/license should be mentioned in the PR description.
- [ ] Implementation must be entirely in Rust.

#### Additional nice-to-haves

- [ ] Minimal or no dependencies on external libraries.
- [ ] No use of unsafe code.

## Version Compatibility

| `image` crate version | Compatible `image-extras` versions |
| --------------------- | ---------------------------------- |
| 0.25.x                | 0.1.x                              |

## Fuzzing

Fuzzing is not a priority for this crate and decoders may panic or worse on
malformed input. Please do not open issues for crashes found by fuzzing,
unless they are memory safety violations, though PRs fixing them are welcome.

This is an intentional tradeoff to balance the inclusion criteria for new
formats with maintainer time and effort.

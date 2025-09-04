# image-extras
Decoding support for additional image formats beyond those provided by the [`image`](https://crates.io/crates/image) crate.

## Supported formats

| Extension | File Format Description |
| --------- | -------------------- |
| PCX | [Wikipedia](https://en.wikipedia.org/wiki/PCX#PCX_file_format) |
| XBM | [Wikipedia](https://en.wikipedia.org/wiki/X_BitMap) |
| XPM | [Wikipedia](https://en.wikipedia.org/wiki/X_PixMap) |

## New Formats

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

## Fuzzing

Fuzzing is not a priority for this crate and decoders may panic or worse on
malformed input. Please do not open issues for crashes found by fuzzing,
unless they are memory safety violations, though PRs fixing them are welcome.

This is an intentional tradeoff to balance the inclusion criteria for new
formats with maintainer time and effort.

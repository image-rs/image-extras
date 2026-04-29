# Release Notes

## 0.1.1

### Additions

- Add ICNS decoder ([#34])
- Add DDS decoder ([#16])

### Changes

- Improve SGI decoding performance ([#43])
- Use `seek_relative` for ICNS parsing ([#38])

### Fixes

- Allow `const` keyword in XPM array types ([#37])
- Support OTB files with 16-bit dimensions ([#43])

[#16]: https://github.com/image-rs/image-extras/pull/16
[#34]: https://github.com/image-rs/image-extras/pull/34
[#37]: https://github.com/image-rs/image-extras/pull/37
[#38]: https://github.com/image-rs/image-extras/pull/38
[#43]: https://github.com/image-rs/image-extras/pull/43

## 0.1.0

Initial release. Supported formats:

- `ora` ([#10])
- `otb` ([#2])
- `pcx` ([`23385a5`])
- `sgi` ([#5])
- `wbmp` ([#2])
- `xbm` ([#4])
- `xpm` ([#4])

[#2]: https://github.com/image-rs/image-extras/pull/2
[#4]: https://github.com/image-rs/image-extras/pull/4
[#5]: https://github.com/image-rs/image-extras/pull/5
[#10]: https://github.com/image-rs/image-extras/pull/10
[`23385a5`]: https://github.com/image-rs/image-extras/commit/23385a59f8951044cf696379b75940621b9ab2c4

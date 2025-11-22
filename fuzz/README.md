# Fuzzing with libfuzzer

For the possibly more up-to-date guide see <https://fuzz.rs/book/cargo-fuzz/setup.html>.

> $ cargo install cargo-fuzz
> $ cargo +nightly fuzz run fuzzer_script_<format>

Fuzzing may progress faster for certain formats if seeded with a dictionary:

> $ cargo +nightly fuzz run fuzzer_script_xbm -- -dict=fuzz/dictionaries/xbm.dict

# Bug reports

As explained in the project [README](../README.md), fuzzing is not a priority for
this crate and decoders may panic or worse on malformed input. Please do not
open issues for crashes found by fuzzing, unless they are memory safety violations,
though PRs fixing them are welcome.

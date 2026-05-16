## 1. Rust Wrapper

- [x] 1.1 Add `crates/uri-sys/` with a C ABI wrapper over the Rust `url` crate.
- [x] 1.2 Expose parsing, normalization, joining, and component accessors needed by workspace code.

## 2. Cosmo Surface

- [x] 2.1 Add `packages/uri-sys/` declarations for the exported ABI.
- [x] 2.2 Add file-URI helpers needed by workspace and document infrastructure.

## 3. Validation

- [x] 3.1 Add parse and normalization tests.
- [x] 3.2 Add file path and file URI round-trip tests.
- [x] 3.3 Add support-library linkage tests through the shared Rust pipeline.

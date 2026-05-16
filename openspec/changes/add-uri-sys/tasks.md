## 1. Rust Wrapper

- [ ] 1.1 Add `crates/uri-sys/` with a C ABI wrapper over the Rust `url` crate.
- [ ] 1.2 Expose parsing, normalization, joining, and component accessors needed by workspace code.

## 2. Cosmo Surface

- [ ] 2.1 Add `packages/uri-sys/` declarations for the exported ABI.
- [ ] 2.2 Add file-URI helpers needed by workspace and document infrastructure.

## 3. Validation

- [ ] 3.1 Add parse and normalization tests.
- [ ] 3.2 Add file path and file URI round-trip tests.
- [ ] 3.3 Add support-library linkage tests through the shared Rust pipeline.

## 1. Rust Wrapper

- [x] 1.1 Add `crates/ureq-sys/` with a C ABI wrapper over blocking `ureq` request execution.
- [x] 1.2 Expose request, response, and error shapes needed by narrow tooling workflows.

## 2. Cosmo Surface

- [x] 2.1 Add `packages/ureq-sys/` declarations for the exported ABI.
- [x] 2.2 Add basic request builder, header, timeout, status, and body access APIs.

## 3. Validation

- [x] 3.1 Add mocked or local HTTP smoke tests.
- [x] 3.2 Add deterministic error mapping tests.
- [x] 3.3 Add support-library linkage tests through the shared Rust pipeline.

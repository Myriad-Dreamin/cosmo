## Why

Repository tools and future generators need deterministic blocking HTTP without pulling an async runtime into the first Cosmo tooling stack. A narrow Rust-backed HTTP bridge is enough for controlled fetch workflows.

## What Changes

- Add `packages/ureq-sys` and `crates/ureq-sys` as a Rust-backed wrapper over `ureq` exposed through a C ABI.
- Expose blocking request construction, headers, status, timeout basics, and body text or bytes retrieval.
- Keep streaming, cookies, proxy management, and async features out of the first slice.

## Capabilities

### New Capabilities

- `ureq-sys`: Defines the Rust-backed blocking HTTP system package for repository tooling.

### Modified Capabilities

- `rust-ffi-support-library-pipeline`: Supplies the shared ABI and build path used by `ureq-sys`.

## Impact

- Future implementation will add `packages/ureq-sys/`, `crates/ureq-sys/`, and Cosmo declarations over the Rust wrapper.
- Makes network-backed tooling possible for metamodel and generator workflows that need network downloads.
- Implementation should land after `add-rust-ffi-support-library-pipeline` and before `add-lsp-types-generator-core-subset`.

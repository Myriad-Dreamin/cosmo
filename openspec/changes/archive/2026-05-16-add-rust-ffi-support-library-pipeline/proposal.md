## Why

`packages/uri-sys` and `packages/ureq-sys` should not each reinvent Rust workspace setup, C ABI conventions, and support-library linking. Cosmo needs one shared pipeline for Rust-backed host capabilities.

## What Changes

- Add a repository-level Rust support-library pipeline for crates under `crates/`.
- Define common C ABI conventions, artifact naming, and support-library identifiers consumed by Cosmo extern bindings.
- Define backend and linker integration for `support-library:*` requirements so later Rust-backed packages share one path.

## Capabilities

### New Capabilities

- `rust-ffi-support-library-pipeline`: Defines shared Rust support-library build, ABI, and link integration for Cosmo packages.

### Modified Capabilities

- `cosmo0-extern-abi-hooks`: Records support-library requirements for Rust-backed extern bindings.
- `cosmo0-cpp-backend`: Consumes support-library artifacts during compile and link orchestration.

## Impact

- Future implementation will add `crates/` workspace scaffolding, Rust build documentation, and backend or linker integration.
- Keeps later Rust-backed packages focused on domain APIs instead of repeated build glue.
- Implementation should land before `add-uri-sys` and `add-ureq-sys`.

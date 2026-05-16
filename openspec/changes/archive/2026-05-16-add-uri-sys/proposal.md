## Why

The language server and package tooling need reliable URI parsing and normalization for file documents, workspace roots, and LSP document identifiers. Cosmo should reuse a proven implementation instead of hand-rolling URI behavior.

## What Changes

- Add `packages/uri-sys` and `crates/uri-sys` as a Rust-backed wrapper over the `url` crate exposed through a C ABI.
- Expose parse, display, join, scheme, authority, path, query, and file-URI helper APIs needed by workspace and editor code.
- Keep broader web-client behavior and unrelated URL utilities out of the first slice.

## Capabilities

### New Capabilities

- `uri-sys`: Defines the Rust-backed URI system package used by workspace and editor infrastructure.

### Modified Capabilities

- `rust-ffi-support-library-pipeline`: Supplies the shared ABI and build path used by `uri-sys`.

## Impact

- Future implementation will add `packages/uri-sys/`, `crates/uri-sys/`, and Cosmo declarations over the Rust wrapper.
- Unblocks stable URI handling for `packages/cosmos` document and workspace code.
- Implementation should land after `add-rust-ffi-support-library-pipeline` and before `create-cosmos-workspace-and-documents`.

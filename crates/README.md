# Cosmo Rust Support Libraries

Rust-backed Cosmo support libraries live under `crates/<support-library-id>/`.
The support-library id is the kebab-case identifier used by Cosmo extern
metadata, for example `support-smoke`, `uri-sys`, or `ureq-sys`.

Each Rust support library uses a Cargo package name matching its id and a Rust
library target named `cosmo_<id-with-underscores>`. For `support-smoke`, the
library target is `cosmo_support_smoke`.

## Artifact Layout

The shared link pipeline expects built artifacts to be staged under:

```text
target/cosmo/support-libraries/<profile>/<support-library-id>/
```

For a static Linux artifact, the expected staged path for `support-smoke` is:

```text
target/cosmo/support-libraries/release/support-smoke/libcosmo_support_smoke.a
```

The C++ backend records `support-library:<id>` requirements from trusted extern
bindings and turns them into a link plan that names these staged artifacts. A
later build step is responsible for copying or symlinking Cargo outputs from
`target/<profile>/` into the shared staged layout before invoking the linker.

## C ABI Rules

Rust support libraries expose only `extern "C"` functions with stable symbol
names prefixed by `cosmo_<id-with-underscores>_`.

Supported shared ABI shapes for this layer are:

- fixed-width integer and floating-point scalars;
- `#[repr(C)]` structs containing supported ABI fields;
- pointers to opaque handles owned by the support library;
- caller-provided output buffers for variable-length data.

Rust-owned allocations must be released by an exported function from the same
support library. Panics must not unwind across the C ABI boundary.

Build the smoke crate with:

```sh
cargo test -p support-smoke
cargo build -p support-smoke --release
```

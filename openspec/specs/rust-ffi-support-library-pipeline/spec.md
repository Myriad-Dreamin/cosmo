# rust-ffi-support-library-pipeline Specification

## Purpose
TBD - created by archiving change add-rust-ffi-support-library-pipeline. Update Purpose after archive.
## Requirements
### Requirement: Repository Rust Support Workspace

The repository SHALL provide a Rust workspace for support libraries under `crates/`. Each Rust-backed support library SHALL live under `crates/<support-library-id>/`, use a Cargo package name matching the support-library id, and expose a Rust library target named `cosmo_<support-library-id-with-hyphens-replaced-by-underscores>`.

#### Scenario: Smoke support library builds in the shared workspace

- **WHEN** the repository Rust workspace is tested for package `support-smoke`
- **THEN** Cargo runs the smoke crate unit tests successfully
- **AND** the crate exposes a `cosmo_support_smoke` Rust library target for static or dynamic support-library artifacts

### Requirement: Support Library Naming And Artifact Layout

The support-library pipeline SHALL define lowercase kebab-case support-library
ids, C symbol prefixes, artifact names, and a shared staged output layout.
Static support-library artifacts SHALL be staged under
`target/cosmo/support-libraries/<profile>/<support-library-id>/`.

#### Scenario: Support library id maps to artifact path

- **WHEN** the support-library id is `uri-sys`
- **THEN** the Rust library target is `cosmo_uri_sys`
- **AND** the C symbol prefix is `cosmo_uri_sys_`
- **AND** the release static artifact path is `target/cosmo/support-libraries/release/uri-sys/libcosmo_uri_sys.a`

#### Scenario: Ureq support library id maps to artifact path

- **WHEN** the support-library id is `ureq-sys`
- **THEN** the Rust library target is `cosmo_ureq_sys`
- **AND** the C symbol prefix is `cosmo_ureq_sys_`
- **AND** the release static artifact path is `target/cosmo/support-libraries/release/ureq-sys/libcosmo_ureq_sys.a`

### Requirement: Shared Rust C ABI Contract

Rust support libraries SHALL expose a narrow C ABI using fixed-width scalar values, `#[repr(C)]` aggregate values, opaque handles for Rust-owned state, and explicit release functions for Rust-owned allocations. Rust panics MUST NOT unwind across the C ABI boundary.

#### Scenario: Smoke support library exports stable C ABI shapes

- **WHEN** the smoke support library is inspected
- **THEN** it exports fixed-width scalar functions using `extern "C"`
- **AND** it exposes an ABI status value using a `#[repr(C)]` struct
- **AND** the ABI status reports the shared Rust FFI ABI version

### Requirement: Support Library Artifact Validation

The support-library pipeline SHALL validate the artifacts supplied to a link plan before host linking. Missing artifacts and ABI-version mismatches SHALL produce stable diagnostics.

#### Scenario: Missing support library artifact is diagnosed

- **WHEN** a link plan requires `support-library:support-smoke`
- **AND** no artifact is supplied for `support-smoke`
- **THEN** validation reports `cosmo0.support-library.missing-artifact`

#### Scenario: Incompatible support library artifact is diagnosed

- **WHEN** a link plan requires `support-library:support-smoke`
- **AND** the supplied artifact reports a different ABI version from the shared Rust FFI ABI version
- **THEN** validation reports `cosmo0.support-library.incompatible-artifact`


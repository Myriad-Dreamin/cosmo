## ADDED Requirements

### Requirement: Blocking Ureq System Package

The repository SHALL provide a Rust-backed `ureq-sys` support library for
blocking HTTP requests used by controlled tooling workflows.

#### Scenario: Ureq support library builds in the shared workspace

- **WHEN** the repository Rust workspace is tested for package `ureq-sys`
- **THEN** Cargo runs the `ureq-sys` unit tests successfully
- **AND** the crate exposes a `cosmo_ureq_sys` Rust library target for static or dynamic support-library artifacts

#### Scenario: Request construction supports headers and timeout

- **WHEN** a caller creates a request with a method and URL
- **THEN** the request can attach named headers
- **AND** the request can set a blocking timeout in milliseconds
- **AND** the request remains an opaque Rust-owned handle until it is executed or released

#### Scenario: Response exposes status, headers, and buffered bodies

- **WHEN** a request receives an HTTP response
- **THEN** the response exposes the numeric status
- **AND** callers can retrieve response header values
- **AND** callers can read the body as text or bounded bytes

#### Scenario: Transport errors map deterministically

- **WHEN** a request fails before receiving a response
- **THEN** `ureq-sys` returns an error handle with a stable numeric kind
- **AND** invalid input and invalid UTF-8 are reported without performing network I/O

#### Scenario: Out-of-scope web client features stay deferred

- **WHEN** the `ureq-sys` API surface is inspected
- **THEN** it does not expose streaming, cookie stores, proxy management, or async execution APIs

### Requirement: Cosmo Ureq System Declarations

The repository SHALL provide `packages/ureq-sys` declarations for the exported
blocking HTTP support-library ABI.

#### Scenario: Declarations record support library linkage

- **WHEN** the Cosmo ureq system declarations bind to exported C ABI symbols
- **THEN** each extern declaration records `supportLibrary = "ureq-sys"`
- **AND** backend support-library link planning can resolve the `ureq-sys` artifact path through the shared Rust pipeline

#### Scenario: Declarations expose safe wrappers instead of raw ABI helpers

- **WHEN** external Cosmo modules import the ureq system package
- **THEN** the public surface provides request, response, error, and owned byte wrapper types
- **AND** raw `unsafe_ureq_sys_*` extern declarations are private implementation details

# uri-sys Specification

## Purpose
TBD - created by archiving change add-uri-sys. Update Purpose after archive.
## Requirements
### Requirement: Rust URI System Package

The repository SHALL provide a Rust-backed `uri-sys` support library for URI
parsing, normalization, joining, component access, and local file URI
conversion.

#### Scenario: URI support library builds in the shared workspace

- **WHEN** the repository Rust workspace is tested for package `uri-sys`
- **THEN** Cargo runs the `uri-sys` unit tests successfully
- **AND** the crate exposes a `cosmo_uri_sys` Rust library target for static or dynamic support-library artifacts

#### Scenario: URI parsing normalizes display text

- **WHEN** a caller parses an absolute URI through `uri-sys`
- **THEN** the support library returns an opaque Rust-owned URI handle
- **AND** displaying that handle returns the normalized URI text produced by the Rust `url` parser

#### Scenario: URI components and joins are exposed

- **WHEN** a caller inspects a parsed URI handle
- **THEN** the support library exposes the scheme, authority, path, and query components
- **AND** joining a relative reference against the handle returns a new normalized URI handle

#### Scenario: Local file paths round trip through file URIs

- **WHEN** a caller converts an absolute local file path to a URI handle
- **THEN** the support library returns a `file:` URI representation
- **AND** converting that URI handle back to a file path returns the original local path text

#### Scenario: URI errors map deterministically

- **WHEN** parsing, joining, file path conversion, or file URI conversion fails
- **THEN** `uri-sys` returns an error handle with a stable numeric kind
- **AND** invalid input and invalid UTF-8 are reported before URL operations are performed
- **AND** Rust panics are contained within the C ABI boundary

### Requirement: Cosmo URI System Declarations

The repository SHALL provide `packages/uri-sys` declarations for the exported
URI support-library ABI.

#### Scenario: Declarations record support library linkage

- **WHEN** the Cosmo URI system declarations bind to exported C ABI symbols
- **THEN** each extern declaration records `supportLibrary = "uri-sys"`
- **AND** backend support-library link planning can resolve the `uri-sys` artifact path through the shared Rust pipeline

#### Scenario: Declarations expose safe wrappers instead of raw ABI helpers

- **WHEN** external Cosmo modules import the URI system package
- **THEN** the public surface provides URI, error, owned byte, and byte slice wrapper types
- **AND** raw `unsafe_uri_sys_*` extern declarations are private implementation details
- **AND** Rust-owned wrapper resources expose `Drop.drop` implementations instead of public `release` methods

#### Scenario: Declarations expose package-consumable parse helpers

- **WHEN** another Cosmo package needs to parse a document URI or convert a file path into a URI handle
- **THEN** `packages/uri-sys` provides public safe top-level helpers for parsing URI strings and constructing URI handles from file path strings
- **AND** those helpers still route through the private raw ABI declarations


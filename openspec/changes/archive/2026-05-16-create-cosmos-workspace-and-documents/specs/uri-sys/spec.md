## MODIFIED Requirements

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

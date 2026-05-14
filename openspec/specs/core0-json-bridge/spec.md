# core0-json-bridge Specification

## Purpose
TBD - created by archiving change add-core0-json-bridge. Update Purpose after archive.
## Requirements
### Requirement: Transitional JSON Bridge

cosmo0 SHALL expose `core0.json` as a source-facing standard capability for transitional JSON loading, rather than as a primitive or runtime descriptor family.

#### Scenario: JSON is available through standard declarations

- **WHEN** cosmo0 source uses `JsonValue`, `Json.parse`, object field access, array access, string access, boolean access, null checks, and numeric literal text access
- **THEN** the source type-checks through the `core0.json` standard API
- **AND** lowered output does not contain `Json` or `JsonValue` descriptor intrinsics

#### Scenario: JSON parse is extern-backed

- **WHEN** cosmo0 source calls `core0_json().parse(text)`
- **THEN** lowering records a trusted `cosmo0.extern.v0` call to `::cosmo0_runtime::json_parse`
- **AND** backend requirements include the matching runtime symbol and `nlohmann::json` include support

#### Scenario: JSON source smoke tests execute through nlohmann

- **WHEN** `packages/cosmoc/src/core0/json_test.cos` is compiled and executed through the C++ backend
- **THEN** valid JSON object, array, string, boolean, null, and number accessors return the expected values
- **AND** invalid JSON returns a parse error result

### Requirement: Syntax JSON Loader

cosmo1 SHALL provide a `syntax/json_loader` source module that maps selected parser JSON into syntax arena data.

#### Scenario: Loader compiles against syntax arenas

- **WHEN** the loader source is compiled with `core0.json`, source spans, and syntax arena declarations
- **THEN** the loader type-checks and lowers successfully
- **AND** syntax nodes are allocated through the arena API rather than JSON descriptor operations


# core0-path-fs Specification

## Purpose
TBD - created by archiving change add-core0-path-fs. Update Purpose after archive.
## Requirements
### Requirement: Minimal Path Filesystem Capability

cosmo0 SHALL expose the `core0.path-fs` standard capability for Stage 1 path values and source-file loading through a narrow filesystem API.

#### Scenario: Stage profile requires the capability

- **WHEN** the `cosmo1.stage1` profile is validated
- **THEN** it requires the `core0.path-fs` std capability
- **AND** missing availability for that capability is reported as a missing std capability

#### Scenario: Path wraps source input names

- **WHEN** Stage 1 source receives an input file name
- **THEN** it can construct a `Path` value from the owned string
- **AND** it can recover the display/source name without exposing host path internals

### Requirement: Source Files Are Read Fallibly

cosmo0 SHALL expose file reading as a standard fallible API returning `Result<String, IoError>` rather than throwing, panicking, or exposing a descriptor operation.

#### Scenario: Source file read succeeds

- **WHEN** Stage 1 source calls `Fs.read_to_string(path)` for an available source file
- **THEN** the API returns `Result<String, IoError>::Ok(contents)`
- **AND** the returned string is the complete source text for that path

#### Scenario: Source file read fails

- **WHEN** Stage 1 source calls `Fs.read_to_string(path)` for a missing or unreadable source file
- **THEN** the API returns `Result<String, IoError>::Err(error)`
- **AND** the error includes a stable message suitable for diagnostics

### Requirement: Filesystem APIs Remain Std-Owned

Filesystem and path operations SHALL be surfaced through `core0.path-fs` standard APIs and extern ABI metadata rather than new runtime descriptor families.

#### Scenario: Filesystem capability is validated as a std capability

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.path-fs`
- **THEN** validation reports a missing std capability diagnostic for `core0.path-fs`

#### Scenario: Filesystem helpers do not become descriptors

- **WHEN** descriptor metadata is inspected for path and filesystem support
- **THEN** `Path`, `IoError`, and `Fs` are not registered descriptor families
- **AND** backend/runtime support is recorded as extern requirements behind the standard API boundary

### Requirement: Cosmo1 Source Loading Uses Path Fs

cosmo1 Stage 1 SHALL provide source-loading helpers layered on `core0.path-fs` and `core0.text`.

#### Scenario: SourceText is loaded by path

- **WHEN** cosmo1 Stage 1 source loads a file path
- **THEN** it calls the `core0.path-fs` standard file-read API
- **AND** it wraps successful contents in `SourceText` using the path display name

#### Scenario: Driver input config owns paths

- **WHEN** the Stage 1 driver records input files
- **THEN** it stores them as `Path` values instead of raw host-specific filesystem handles


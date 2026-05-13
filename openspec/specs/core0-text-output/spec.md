# core0-text-output Specification

## Purpose
TBD - created by archiving change add-core0-text-output. Update Purpose after archive.
## Requirements
### Requirement: Minimal Text Output Capability

cosmo0 SHALL expose the `core0.text-output` standard capability for deterministic Stage 1 text sinks.

#### Scenario: Stage profile requires the capability

- **WHEN** the `cosmo1.stage1` profile is validated
- **THEN** it requires the `core0.text-output` std capability
- **AND** missing availability for that capability is reported as a missing std capability

#### Scenario: Source writes text to stdout

- **WHEN** Stage 1 source has an owned `String`
- **THEN** it can write that string through a `TextWriter`
- **AND** it can write a complete line through the same sink
- **AND** stdout helper functions are available for smoke programs and diagnostics

### Requirement: Text Output APIs Remain Std-Owned

Text output operations SHALL be surfaced through `core0.text-output` standard APIs and extern ABI metadata rather than new runtime descriptor families.

#### Scenario: Text output helpers do not become descriptors

- **WHEN** descriptor metadata is inspected for text output support
- **THEN** `TextWriter`, `Stdout`, `Stderr`, `Output`, and `Console` are not registered descriptor families
- **AND** backend/runtime support is recorded as extern requirements behind the standard API boundary

#### Scenario: Trusted output externs record runtime requirements

- **WHEN** Stage 1 source lowers `print` or `println`
- **THEN** the lowered call records the matching `cosmo0.extern.v0` runtime symbol
- **AND** the backend records the required C++ runtime include support

### Requirement: Cosmo1 Diagnostics Render To Text Sinks

cosmo1 Stage 1 SHALL keep diagnostic formatting in driver code and write rendered diagnostics through `core0.text-output`.

#### Scenario: Diagnostic rendering is deterministic

- **WHEN** cosmo1 renders a diagnostic with labels
- **THEN** the output text contains severity, message, and labels in source order
- **AND** repeated rendering of the same diagnostic returns the same text

#### Scenario: Diagnostic rendering uses an output sink

- **WHEN** cosmo1 writes a diagnostic
- **THEN** it writes the formatted text through a `TextWriter`
- **AND** formatting behavior is not moved into the standard output API


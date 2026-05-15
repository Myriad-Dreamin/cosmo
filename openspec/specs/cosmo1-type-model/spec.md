# cosmo1-type-model Specification

## Purpose
TBD - created by archiving change add-cosmo1-type-model. Update Purpose after archive.
## Requirements
### Requirement: Cosmo1 Type Data

cosmo1 SHALL define type data for primitive types, user class types, function types, reference types, never/error types, and stable type identifiers needed by parser self-compile.

#### Scenario: Parser primitive types are represented

- **WHEN** cosmo1 creates types for the parser-source subset
- **THEN** it can represent `Unit`, `Bool`, `String`, `usize`, `u8`, and other primitive types used by `parser.cos`

#### Scenario: User and callable types are represented

- **WHEN** cosmo1 models parser classes and functions
- **THEN** it can represent user class types, function parameter and return types, immutable references, and mutable references

### Requirement: Type Comparison And Display

cosmo1 SHALL compare and display parser-source types deterministically.

#### Scenario: Type diagnostics can render names

- **WHEN** a later type-checking stage reports a mismatch
- **THEN** the expected and actual cosmo1 types have stable display text


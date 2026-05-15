# cosmo1-basic-expression-lowering Specification

## Purpose
TBD - created by archiving change lower-cosmo1-basic-expressions-to-ir. Update Purpose after archive.
## Requirements
### Requirement: Basic Expression Lowering

cosmo1 SHALL lower basic typed parser-source expressions into verifier-accepted IR.

#### Scenario: Simple body lowers

- **WHEN** a typed function body contains literals, locals, names, assignment, direct calls, arithmetic, comparison, equality, or returns
- **THEN** cosmo1 lowers the body into IR operations and terminators that pass verification

#### Scenario: Lowering is deterministic

- **WHEN** equivalent typed expression input is lowered repeatedly
- **THEN** local ids, temporary ids, block order, and debug output are stable


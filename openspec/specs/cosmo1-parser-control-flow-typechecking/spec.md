# cosmo1-parser-control-flow-typechecking Specification

## Purpose
TBD - created by archiving change typecheck-cosmo1-parser-control-flow. Update Purpose after archive.
## Requirements
### Requirement: Parser Control Flow Typing

cosmo1 SHALL type-check `if`, `else`, `while`, nested scopes, early returns, and function return compatibility for the supported `parser.cos` source subset.

#### Scenario: Conditions require Bool

- **WHEN** source uses `if` or `while`
- **THEN** cosmo1 requires the condition expression to have type `Bool`

#### Scenario: Returns match function type

- **WHEN** source returns from a function or method
- **THEN** cosmo1 validates that the returned expression is assignable to the declared return type

### Requirement: Parser Source Type-Checks

cosmo1 SHALL accept complete type checking of `packages/cosmoc/src/parser.cos` after the preceding type-checking slices exist.

#### Scenario: Parser source accepted by type checker

- **WHEN** cosmo1 type-checks the supported `parser.cos` source
- **THEN** the result is a deterministic typed module suitable for lowering


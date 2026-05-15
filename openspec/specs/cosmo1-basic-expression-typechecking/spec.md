# cosmo1-basic-expression-typechecking Specification

## Purpose
TBD - created by archiving change typecheck-cosmo1-basic-expressions. Update Purpose after archive.
## Requirements
### Requirement: Basic Expression Typing

cosmo1 SHALL type-check basic parser-source expressions and produce typed expression data for later compiler stages.

#### Scenario: Simple expressions are typed

- **WHEN** cosmo1 checks literals, names, locals, blocks, assignments, returns, arithmetic, comparison, and equality expressions
- **THEN** it assigns each accepted expression a cosmo1 type

#### Scenario: Type mismatch is diagnosed

- **WHEN** an expression type is not assignable to the expected type
- **THEN** cosmo1 reports a type mismatch diagnostic with expected and actual type display text

### Requirement: Local Mutation Rules

cosmo1 SHALL enforce mutation rules for basic local assignment.

#### Scenario: Immutable local assignment is rejected

- **WHEN** source assigns to an immutable local binding
- **THEN** cosmo1 reports an assignment diagnostic before lowering


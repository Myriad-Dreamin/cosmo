# cosmo0-canonical-loop-elaboration Specification

## Purpose
TBD - created by archiving change preserve-cosmo0-structured-loops. Update Purpose after archive.
## Requirements
### Requirement: Canonical Source Loop Representation

The cosmo0 elaborator SHALL represent source `loop`, `while`, and restricted
`for` constructs with a single canonical untyped loop node containing
prologue, condition, body, and epilogue slots.

#### Scenario: Infinite loop elaborates to canonical loop

- **WHEN** the source contains `loop { body }`
- **THEN** elaboration produces a canonical loop with an always-true condition
- **AND** the loop body contains the elaborated `body`

#### Scenario: While loop elaborates to canonical loop

- **WHEN** the source contains `while cond { body }`
- **THEN** elaboration produces a canonical loop whose condition contains the
  elaborated `cond`
- **AND** the loop body contains the elaborated `body`

#### Scenario: For loop elaborates to canonical loop

- **WHEN** the source contains `for item in iter { body }`
- **THEN** elaboration produces a canonical loop that preserves the item binding
  name, the elaborated iterator expression, and the elaborated body

### Requirement: Type-Driven For Desugaring Is Deferred

The cosmo0 elaborator SHALL NOT resolve `for` item types or lower `for`
iteration into descriptor operations. It SHALL preserve enough canonical loop
metadata for the typer and lowerer to perform those type-driven steps.

#### Scenario: For elaboration does not synthesize descriptor calls

- **WHEN** elaboration processes `for item in values { body }`
- **THEN** the untyped result contains the iterator expression and item binding
  name
- **AND** it does not contain synthesized `iter_has_next` or `iter_next`
  descriptor operations

### Requirement: Canonical Loop Control Preservation

The cosmo0 elaborator SHALL preserve `break` and `continue` expressions inside
canonical loop bodies without rewriting them to labels or jumps.

#### Scenario: Loop control survives elaboration

- **WHEN** a source loop body contains `break` or `continue`
- **THEN** the canonical loop body contains the corresponding untyped loop
  control expression


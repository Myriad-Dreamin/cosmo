# cosmo0-structured-loop-lir Specification

## Purpose
TBD - created by archiving change preserve-cosmo0-structured-loops. Update Purpose after archive.
## Requirements
### Requirement: Typed Canonical Loops

The cosmo0 typer SHALL type canonical source loops as a single typed loop
construct with prologue, condition, body, epilogue, and `Unit` result type.

#### Scenario: While condition must be Bool

- **WHEN** a canonical loop has a source condition expression
- **THEN** the typer checks that expression against `Bool`
- **AND** the typed loop has result type `Unit`

#### Scenario: For item binding is typed in body scope

- **WHEN** a canonical loop represents `for item in iter { body }`
- **THEN** the typer infers the iterable item type from `iter`
- **AND** the loop body scope binds `item` with that item type
- **AND** the typed loop has result type `Unit`

### Requirement: Structured LIR Loop Representation

The cosmo0 lowerer SHALL preserve supported typed loops as structured LIR loop
statements instead of lowering them immediately to header, body, continue, and
exit blocks.

#### Scenario: While lowers to structured LIR loop

- **WHEN** the lowerer processes a typed canonical while loop
- **THEN** the produced LIR function body contains a structured loop statement
  with a boolean condition
- **AND** it does not require synthetic header, body, continue, and exit labels
  for that loop

#### Scenario: Infinite loop lowers to structured LIR loop

- **WHEN** the lowerer processes a typed canonical infinite loop
- **THEN** the produced LIR function body contains a structured loop statement
  with an always-true condition

#### Scenario: For loop lowers descriptor iteration into structured loop

- **WHEN** the lowerer processes a typed canonical `for` loop over a supported
  iterable descriptor
- **THEN** the structured LIR loop evaluates the iterator expression once before
  the loop
- **AND** the loop condition uses the descriptor `iter_has_next` operation
- **AND** the loop body binds the item using the descriptor `iter_next`
  operation before lowering the user body

### Requirement: Structured Loop Control Semantics

Structured LIR loops SHALL define `break` as leaving the nearest enclosing loop
without running that loop's epilogue, and `continue` as running that loop's
epilogue before proceeding to the next condition check.

#### Scenario: Break skips epilogue

- **WHEN** a structured loop body executes `break`
- **THEN** control leaves the nearest enclosing structured loop
- **AND** that loop's epilogue is not executed for the broken iteration

#### Scenario: Continue runs epilogue

- **WHEN** a structured loop body executes `continue`
- **THEN** that loop's epilogue executes before the next condition check
- **AND** control remains within the nearest enclosing structured loop

### Requirement: Structured LIR Loop Checking

The LIR type checker SHALL validate structured loop bodies, loop conditions,
and loop-control placement.

#### Scenario: Structured loop condition is checked as Bool

- **WHEN** a structured LIR loop has a condition value
- **THEN** the LIR type checker requires that condition value to have type
  `Bool`

#### Scenario: Break outside loop is rejected

- **WHEN** a structured LIR function body contains `break` outside any
  structured loop
- **THEN** the LIR type checker rejects the function body with a diagnostic

#### Scenario: Continue outside loop is rejected

- **WHEN** a structured LIR function body contains `continue` outside any
  structured loop
- **THEN** the LIR type checker rejects the function body with a diagnostic


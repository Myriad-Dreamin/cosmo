## MODIFIED Requirements

### Requirement: LIR Declaration And Body Coverage

The cosmo0 C++ backend SHALL emit C++ for LIR type aliases, classes, fields,
variants, globals, functions, methods, locals, assignments, field reads and
writes, direct calls, lowered method calls, descriptor calls, structured loops,
structured loop controls, returns, branches, conditional branches, unreachable
exits, and error exits.

#### Scenario: Representative checked LIR emits C++

- **WHEN** a checked LIR module contains user declarations, variants, function
  bodies, descriptor calls, structured loops, and lowered fallback control flow
- **THEN** the backend emits C++ declarations and function bodies for every
  supported LIR construct

## ADDED Requirements

### Requirement: Structured Loop While Emission

The cosmo0 C++ backend SHALL emit supported structured LIR loops as C++
`while` statements and SHALL use native C++ `break` and `continue` for
structured loop controls.

#### Scenario: Structured while emits C++ while

- **WHEN** a checked LIR function contains a structured loop with a boolean
  condition and a body
- **THEN** the backend emits a C++ `while` statement for that loop
- **AND** the emitted loop does not require a synthetic label/goto cycle for the
  loop header, body, continue, and exit edges

#### Scenario: Continue emits epilogue before continue

- **WHEN** a checked LIR structured loop has a non-empty epilogue and the body
  contains structured `continue`
- **THEN** the backend emits the epilogue before the native C++ `continue`
  statement for that loop control

#### Scenario: Break emits native break

- **WHEN** a checked LIR structured loop body contains structured `break`
- **THEN** the backend emits native C++ `break`
- **AND** it does not emit that loop's epilogue before the `break`

### Requirement: Flat Control Flow Fallback

The cosmo0 C++ backend SHALL keep emitting flat block/branch LIR with labels
and `goto` for supported constructs that have not yet been represented as
structured LIR.

#### Scenario: Fallback branches still emit

- **WHEN** a checked LIR function contains supported flat branches or
  conditional branches outside structured loops
- **THEN** the backend emits equivalent C++ label and branch control flow

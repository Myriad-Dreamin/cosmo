# cosmo0-cpp-backend Specification

## Purpose
TBD - created by archiving change add-cosmo0-cpp-backend. Update Purpose after archive.
## Requirements
### Requirement: Verified LIR Backend Boundary

The cosmo0 C++ backend SHALL accept verified cosmo0 LIR modules and SHALL reject
unverified or structurally invalid LIR at the backend boundary.

#### Scenario: Valid LIR emits C++

- **WHEN** a caller passes a LIR module accepted by the cosmo0 LIR checker
- **THEN** the backend emits deterministic C++ output for that module

#### Scenario: Invalid LIR is rejected

- **WHEN** a caller passes a LIR module rejected by the cosmo0 LIR checker
- **THEN** the backend returns compile-phase diagnostics and emits no C++ output

### Requirement: Deterministic C++ Emission

The cosmo0 C++ backend SHALL emit stable namespaces, symbols, declaration order,
local names, block labels, runtime includes, and support code for equivalent LIR
inputs.

#### Scenario: Equivalent LIR order emits identical bytes

- **WHEN** two LIR modules contain equivalent declarations, locals, fields,
  variants, and blocks in different input orders
- **THEN** the backend output is byte-for-byte identical

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

### Requirement: Runtime Descriptor Support

The cosmo0 C++ backend SHALL emit runtime includes or support code for every
lowered descriptor operation it supports and SHALL diagnose unsupported
descriptor operations.

#### Scenario: Descriptor support is emitted once

- **WHEN** a LIR module uses the same supported descriptor operation multiple
  times
- **THEN** the backend emits the required runtime include or support code once
  for the output unit

#### Scenario: Unsupported descriptor is diagnosed

- **WHEN** a LIR module uses a descriptor operation that the backend cannot emit
- **THEN** the backend returns a compile-phase unsupported-descriptor diagnostic
  and emits no C++ output

### Requirement: Initial Compile Acceptance Targets

The cosmo0 C++ backend SHALL compile the initial library and executable targets
needed to prove the backend path is usable.

#### Scenario: parser.cos compiles as a library

- **WHEN** `packages/cosmoc/src/parser.cos` is compiled through the cosmo0
  backend
- **THEN** the backend emits C++ that the configured C++ toolchain accepts as a
  library-style output without requiring a `main` function

#### Scenario: HelloWorld compiles as an executable

- **WHEN** `samples/HelloWorld/main.cos` is compiled through the cosmo0 backend
- **THEN** the backend emits C++ that the configured C++ toolchain accepts as an
  executable program

### Requirement: Full Cosmo Backend Isolation

Adding the cosmo0 C++ backend SHALL NOT change the existing full Cosmo compiler
backend or require full Cosmo code generation to emit cosmo0 C++.

#### Scenario: cosmo0 backend does not call full CodeGen

- **WHEN** cosmo0 emits C++ for checked LIR
- **THEN** the emission path does not depend on the full Cosmo `CodeGen`
  implementation

### Requirement: Support Library Link Planning

The cosmo0 C++ backend SHALL consume `support-library:<id>` backend requirements by producing a support-library link plan. The link plan SHALL contain the expected staged artifact path for every required support library and SHALL preserve support-library requirements separately from runtime symbols, includes, and descriptors.

#### Scenario: Support library requirement becomes link argument

- **WHEN** C++ emission sees a direct C extern call requiring `support-library:support-smoke`
- **THEN** backend requirements include `support-library:support-smoke`
- **AND** the support-library link plan includes `target/cosmo/support-libraries/release/support-smoke/libcosmo_support_smoke.a`
- **AND** runtime symbol requirements remain represented separately from support-library requirements

#### Scenario: Invalid hand-built support library requirement is diagnosed

- **WHEN** a checked LIR module contains an invalid support-library requirement value
- **THEN** C++ backend emission fails with `cosmo0.support-library.invalid-id`
- **AND** no C++ output is treated as a successful backend result

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


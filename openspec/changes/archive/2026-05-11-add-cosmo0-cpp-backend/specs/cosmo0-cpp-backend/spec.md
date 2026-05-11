## ADDED Requirements

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
writes, direct calls, lowered method calls, descriptor calls, returns,
branches, conditional branches, unreachable exits, and error exits.

#### Scenario: Representative checked LIR emits C++

- **WHEN** a checked LIR module contains user declarations, variants, function
  bodies, descriptor calls, and lowered control flow
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

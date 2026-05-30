## ADDED Requirements

### Requirement: Compile-Time Evaluation Boundary

cosmo0 SHALL provide a controlled compile-time evaluation boundary for macro
providers and future const evaluation without executing arbitrary runtime
programs.

#### Scenario: Compiler-hosted provider uses the same boundary

- **WHEN** the first implementation invokes a compiler-hosted macro provider
- **THEN** the provider receives only the explicit macro input admitted by the compile-time evaluation boundary
- **AND** the provider output is validated through the ordinary macro output contract

#### Scenario: Self-hosted provider uses interpreter boundary

- **WHEN** a later self-hosted macro provider is compiled for macro execution
- **THEN** cosmo0 runs it through a dedicated compile-time evaluator or interpreter over macro IR
- **AND** it does not execute the target program binary or depend on the target C++ backend

### Requirement: Compile-Time Value Model

cosmo0 SHALL define a finite compile-time value model for attribute evaluation
and macro provider inputs.

#### Scenario: Attribute expression evaluates to const value

- **WHEN** a macro attribute argument contains admitted literal, path, type, array, record, or keyed argument syntax
- **THEN** compile-time evaluation lowers it to a `ConstValue` such as Bool, String, integer, TypeRef, PathRef, array, record, or tag data

#### Scenario: Unsupported value shape is diagnosed

- **WHEN** an attribute argument requires an unsupported expression form, runtime call, mutation, IO, or non-terminating computation
- **THEN** compile-time evaluation reports an unsupported compile-time expression diagnostic
- **AND** the macro provider does not receive a fabricated value

### Requirement: Expression Kind Separation

cosmo0 SHALL distinguish parsed source expressions, attribute expressions,
compile-time values, generated expressions, and typed expressions in macro APIs.

#### Scenario: Attribute API exposes AttrExpr or ConstValue

- **WHEN** a macro provider inspects field attributes
- **THEN** it sees restricted `AttrExpr` syntax or evaluated `ConstValue` data
- **AND** it does not receive arbitrary unchecked `SourceExpr` bodies by default

#### Scenario: Generated expression is checked later

- **WHEN** a macro provider emits a `GeneratedExpr` inside a generated declaration
- **THEN** ordinary type checking validates the generated expression after macro expansion
- **AND** the provider does not mark the expression as trusted `TypedExpr`

### Requirement: Compile-Time Determinism Controls

cosmo0 compile-time evaluation SHALL be deterministic and bounded.

#### Scenario: Evaluation budget is exceeded

- **WHEN** a self-hosted macro provider exceeds the configured step, recursion, or allocation budget
- **THEN** compile-time evaluation reports a deterministic budget diagnostic
- **AND** expansion does not continue with partial unchecked output

#### Scenario: Ambient side effect is requested

- **WHEN** compile-time evaluation attempts undeclared filesystem access, command execution, environment inspection, network IO, time access, randomness, or target runtime execution
- **THEN** evaluation reports a compile-time capability diagnostic
- **AND** the package result does not depend on ambient host state

### Requirement: Interpreter And Runtime Separation

cosmo0 SHALL keep the compile-time interpreter/evaluator separate from runtime
execution and backend emission.

#### Scenario: Backend is unavailable during macro evaluation

- **WHEN** a package uses only compile-time macro providers and the target backend has not emitted executable code yet
- **THEN** macro evaluation can still run through the compile-time evaluator
- **AND** it does not require compiling or launching the package executable

#### Scenario: Runtime-only API is called at compile time

- **WHEN** a macro provider attempts to call a runtime-only API that is not admitted by the compile-time capability set
- **THEN** compile-time evaluation rejects the call with a capability diagnostic

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

### Requirement: Macro Expr Is Untyped Source Expression

cosmo0 SHALL define macro `Expr[T = Untyped]` as an untyped source-expression
value.

#### Scenario: Expression macro receives untyped expression values

- **WHEN** macro expansion invokes an expression macro for a parsed source call
- **THEN** the provider input represents each argument as `Expr[Untyped]`
- **AND** `Untyped` is a macro-level phase marker, not an object-language runtime type
- **AND** `Expr[Untyped]` is not a trusted typed expression artifact

#### Scenario: Untyped marker is not arbitrary type evidence

- **WHEN** a macro provider receives or produces `Expr[Untyped]`
- **THEN** `Untyped` denotes that the expression has not been checked by the ordinary typer
- **AND** the provider cannot use the `T` parameter as evidence for an arbitrary object-language result type

#### Scenario: Expression macro output is checked later

- **WHEN** a macro provider emits `Expr[Untyped]` as expression output or inside a generated declaration
- **THEN** ordinary type checking validates that expression after macro expansion
- **AND** the provider does not mark the expression as trusted or already typed

#### Scenario: Typed facts are inspected through typer APIs

- **WHEN** a macro provider needs the type of an expression value admitted for typed inspection
- **THEN** cosmo0 exposes the information through a bounded typer-phase inspector such as `Type.of(expr)`
- **AND** the inspector returns stable type facts rather than a mutable or constructible `TypedExpr` tree

### Requirement: C++ JIT Support Uses cosmo-jit-sys

cosmo0 SHALL route admitted C++ compile-time support through `cosmo-jit-sys`,
backed by clang-repl, rather than arbitrary command execution or target runtime
execution.

#### Scenario: Provider requests C++ JIT support

- **WHEN** a macro provider is admitted to request C++ JIT support
- **THEN** it invokes `cosmo-jit-sys` with declared headers, include paths, libraries, generated wrapper or probe snippets, expected symbols, target settings, and toolchain identity
- **AND** `cosmo-jit-sys` returns structured diagnostics, support binding metadata, opaque symbol tokens, and generated artifact summaries
- **AND** generated Cosmo output still enters ordinary validation and type checking

#### Scenario: Undeclared native capability is requested

- **WHEN** a macro provider requests undeclared include paths, undeclared dynamic libraries, environment access, time, randomness, command execution, or target program execution through the C++ JIT path
- **THEN** compile-time evaluation reports a capability diagnostic
- **AND** the package result does not depend on that ambient host state

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

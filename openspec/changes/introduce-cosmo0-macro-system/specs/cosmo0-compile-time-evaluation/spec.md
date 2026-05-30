## ADDED Requirements

### Requirement: Compile-Time Evaluation Boundary

cosmo0 SHALL provide a controlled compile-time evaluation boundary for macro
providers and future const evaluation without approximating C++ execution in a
JavaScript host model.

#### Scenario: Compiler-hosted provider uses the same boundary

- **WHEN** the first implementation invokes a compiler-hosted macro provider
- **THEN** the provider receives only the explicit macro input admitted by the compile-time evaluation boundary
- **AND** the provider output is validated through the ordinary macro output contract

#### Scenario: Self-hosted provider uses JIT boundary

- **WHEN** a later self-hosted macro provider is compiled for macro execution
- **THEN** cosmo0 may run it through `cosmo-jit-sys` when it needs C++ compile-time execution
- **AND** generated Cosmo output still returns through the macro output protocol before ordinary validation and type checking

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

### Requirement: C++ Compile-Time Execution Uses cosmo-jit-sys

cosmo0 SHALL route C++ compile-time provider execution through
`cosmo-jit-sys`, backed by clang-repl, rather than a JavaScript host JIT or
handwritten C++ layout emulation.

#### Scenario: Provider executes C++ at compile time

- **WHEN** a macro provider needs to import C++ types, instantiate templates, inspect C++ layout, or execute provider C++ code during compilation
- **THEN** it runs through a `cosmo-jit-sys` clang-repl session with C++ imports, headers, provider source or generated snippets, target settings, and toolchain identity
- **AND** `cosmo-jit-sys` returns structured diagnostics, macro output serialization, requested C++ type facts, support binding metadata, and generated artifact summaries
- **AND** generated Cosmo output still enters ordinary validation and type checking

#### Scenario: JavaScript host layout approximation is rejected

- **WHEN** a provider needs C++ struct, class, template, or ABI-visible type facts
- **THEN** cosmo0 obtains those facts from Clang through `cosmo-jit-sys`
- **AND** it does not model C++ structs as JavaScript objects, typed arrays, or handwritten layout tables
- **AND** it does not guess C++ padding, alignment, bit-field placement, overload resolution, or template instantiation in JavaScript

### Requirement: Compile-Time Determinism Controls

cosmo0 macro output SHALL be deterministic for repeated evaluation of the same
cosmo0 input and declared C++ JIT execution context.

#### Scenario: Non-pure provider changes macro output

- **WHEN** a provider uses hidden mutable state, time, randomness, filesystem contents, environment state, or other ambient effects to produce different macro output for the same cosmo0 input
- **THEN** package behavior is undefined
- **AND** an implementation may diagnose the violation when it can observe it

#### Scenario: Repeated pure evaluation

- **WHEN** a provider is evaluated repeatedly with the same provider identity, macro input, C++ imports, provider source, target settings, and toolchain identity
- **THEN** it produces the same macro output, diagnostics, and generated artifact summary

### Requirement: Provider Execution And Target Runtime Separation

cosmo0 SHALL keep provider host execution separate from target package runtime
execution and backend mutation.

#### Scenario: Backend is unavailable during macro evaluation

- **WHEN** a package uses compile-time macro providers and the target backend has not emitted the package executable yet
- **THEN** macro evaluation can still run through `cosmo-jit-sys`
- **AND** it does not require compiling or launching the package executable

#### Scenario: Provider attempts direct compiler mutation

- **WHEN** a provider executes C++ code during compilation
- **THEN** it cannot directly patch typed modules, lowering IR, backend output, or compiler global state
- **AND** it must affect the package only by returning validated macro output

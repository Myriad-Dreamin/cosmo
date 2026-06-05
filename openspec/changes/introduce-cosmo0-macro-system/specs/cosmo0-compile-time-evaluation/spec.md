## ADDED Requirements

### Requirement: Compile-Time Evaluation Boundary

cosmo0 SHALL provide a controlled compile-time evaluation boundary for macro
functions without approximating C++ execution in a JavaScript host model.

#### Scenario: Compiler-hosted provider uses the same boundary

- **WHEN** the first implementation invokes a compiler-hosted macro function
- **THEN** the provider receives only the explicit macro input admitted by the compile-time evaluation boundary
- **AND** the provider output is validated through the ordinary macro output contract

#### Scenario: Self-hosted provider uses CTE compile boundary

- **WHEN** a later self-hosted macro function is compiled for macro execution
- **THEN** cosmo0 may run it through cosmo0 eval when it needs C++ compile-time execution
- **AND** generated Cosmo output still returns through the macro output protocol before ordinary validation and type checking

### Requirement: Macro Function Input And Output Records

cosmo0 SHALL execute macro functions through explicit serialized input and
output records.

#### Scenario: Compiler selects macro function input

- **WHEN** a macro function is invoked
- **THEN** cosmo0 provides a serialized input record containing provider identity, source package identity, macro target or call identity, compiler-selected reflection facts, admitted attributes and defaults, expression fragments when applicable, source spans, hygiene metadata, and C++ execution context
- **AND** the provider does not receive raw compiler mutation handles

#### Scenario: Macro function output returns to ordinary checking

- **WHEN** a macro function returns generated declarations, generated expression fragments, consumed attributes, diagnostics, generated-source summary, or native support binding metadata
- **THEN** cosmo0 treats that data as serialized macro output
- **AND** generated declarations and expression fragments still enter ordinary validation, name resolution, type checking, and lowering

### Requirement: Macro Expr Is Untyped Source Expression

cosmo0 SHALL define macro `Expr[T = Untyped]` as an untyped source-expression
value.

#### Scenario: Expression macro receives untyped expression values

- **WHEN** macro expansion invokes an expression macro for a parsed source call
- **THEN** the provider input represents each argument as `Expr[Untyped]`
- **AND** `Untyped` is a macro-level phase marker, not an object-language runtime type
- **AND** `Expr[Untyped]` is not a trusted typed expression artifact

#### Scenario: Untyped marker is not arbitrary type evidence

- **WHEN** a macro function receives or produces `Expr[Untyped]`
- **THEN** `Untyped` denotes that the expression has not been checked by the ordinary typer
- **AND** the provider cannot use the `T` parameter as evidence for an arbitrary object-language result type

#### Scenario: Expression macro output is checked later

- **WHEN** a macro function emits `Expr[Untyped]` as expression output or inside a generated declaration
- **THEN** ordinary type checking validates that expression after macro expansion
- **AND** the provider does not mark the expression as trusted or already typed

#### Scenario: Typed facts are inspected through typer APIs

- **WHEN** a macro function needs the type of an expression value admitted for typed inspection
- **THEN** cosmo0 exposes the information through a bounded typer-phase inspector such as `Type.of(expr)`
- **AND** the inspector returns stable type facts rather than a mutable or constructible `TypedExpr` tree

### Requirement: C++ Compile-Time Execution Uses cosmo0 Eval

cosmo0 SHALL route C++ compile-time provider execution through
cosmo0 eval, backed by ordinary Clang provider-entry compilation and
PCH/precompiled context reuse, rather than clangInterpreter, cosmoc, a
JavaScript host JIT, or handwritten C++ layout emulation.

#### Scenario: Provider executes C++ at compile time

- **WHEN** a macro function needs to import C++ types, instantiate templates, inspect C++ layout, or execute provider C++ code during compilation
- **THEN** it runs through a cosmo0 eval provider-entry compile request with C++ imports, headers, provider source or generated entry snippets, target settings, precompiled context key, and toolchain identity
- **AND** cosmo0 eval returns structured diagnostics, macro output serialization, requested C++ type facts, support binding metadata, generated artifact summaries, and precompiled context cache summaries
- **AND** generated Cosmo output still enters ordinary validation and type checking

#### Scenario: clangInterpreter execution is rejected

- **WHEN** a macro function needs C++ compile-time execution
- **THEN** cosmo0 does not use clang-repl or clangInterpreter as the semantic execution substrate
- **AND** provider entry functions are compiled through the Clang compile path

#### Scenario: JavaScript host layout approximation is rejected

- **WHEN** a macro function needs C++ struct, class, template, or ABI-visible type facts
- **THEN** cosmo0 obtains those facts from ordinary Clang compile facts through cosmo0 eval
- **AND** it does not model C++ structs as JavaScript objects, typed arrays, or handwritten layout tables
- **AND** it does not guess C++ padding, alignment, bit-field placement, overload resolution, or template instantiation in JavaScript

### Requirement: Compile-Time Determinism Controls

cosmo0 macro output SHALL be deterministic for repeated evaluation of the same
cosmo0 input and declared C++ compile-time execution context.

#### Scenario: Non-pure provider changes macro output

- **WHEN** a macro function uses hidden mutable state, time, randomness, filesystem contents, environment state, or other ambient effects to produce different macro output for the same cosmo0 input
- **THEN** package behavior is undefined
- **AND** an implementation may diagnose the violation when it can observe it

#### Scenario: Repeated pure evaluation

- **WHEN** a macro function is evaluated repeatedly with the same provider identity, macro input, C++ imports, provider source, target settings, and toolchain identity
- **THEN** it produces the same macro output, diagnostics, and generated artifact summary

### Requirement: Provider Execution And Target Runtime Separation

cosmo0 SHALL keep provider host execution separate from target package runtime
execution and backend mutation.

#### Scenario: Backend is unavailable during macro evaluation

- **WHEN** a package uses compile-time macro providers and the target backend has not emitted the package executable yet
- **THEN** macro evaluation can still run through cosmo0 eval
- **AND** it does not require compiling or launching the package executable

#### Scenario: Provider attempts direct compiler mutation

- **WHEN** a macro function executes C++ code during compilation
- **THEN** it cannot directly patch typed modules, lowering IR, backend output, or compiler global state
- **AND** it must affect the package only by returning validated macro output

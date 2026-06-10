# cosmo0-macro-expansion Specification

## Purpose
TBD - created by archiving change introduce-cosmo0-macro-system. Update Purpose after archive.
## Requirements
### Requirement: Deterministic Macro Expansion Points

cosmo0 SHALL expand macro invocations at deterministic compiler integration
points rather than requiring every macro in a module to expand before checking
can begin. Expression macro calls SHALL expand while the expression checker is
checking that call. The generated `Expr[Untyped]` SHALL then be checked in the
same lexical scope, function context, and expected type as the original macro
call.

Declaration and derive macro output, when admitted by a capability, SHALL be
integrated before later compiler facts that depend on the generated artifacts.

#### Scenario: Expression macro expands at the checked call

- **WHEN** the expression checker is checking `val answer: u8 = example.answer()` and the call target selects an expression macro provider
- **THEN** the checker invokes the provider at that expression site
- **AND** the provider output is rechecked as an ordinary expression with expected type `u8`
- **AND** the module is not pre-expanded in a separate whole-module expression macro pass

#### Scenario: Derive attaches trait implementation before trait use

- **WHEN** a class annotated with `@derive(example.MakeParse)` generates an implementation of an existing trait for that class
- **THEN** later source in the same package can type-check uses that require the trait implementation
- **AND** the derive output does not add a new top-level name or target member to ordinary name resolution

#### Scenario: Repeated expansion is stable

- **WHEN** the same package is checked twice with the same macro providers and source inputs
- **THEN** macro expansion produces the same generated artifacts, diagnostics, and generated-source summary order

### Requirement: Macro Attribute Preservation

cosmo0 SHALL preserve structured macro attributes on supported declaration
shapes until an admitted declaration or derive macro expansion consumes or
rejects them.

#### Scenario: Field attribute reaches derive provider

- **WHEN** a class field is annotated with `@arg(long = "package", short = "p")`
- **THEN** the derive provider receives an attribute record containing the path `arg`, keyed string values `long` and `short`, and the field source span

#### Scenario: Malformed attribute is diagnosed

- **WHEN** a macro attribute uses an unsupported argument shape for the selected provider
- **THEN** expansion reports a macro diagnostic at the attribute span
- **AND** generated artifacts for that macro invocation are not accepted

### Requirement: Macro Provider Registry

cosmo0 SHALL resolve macro provider paths through a deterministic provider
registry before invoking an expression, derive, or attribute macro.

#### Scenario: Expression provider path resolves

- **WHEN** source uses `example.answer()` and the selected checker profile registers `example.answer` as an expression macro provider
- **THEN** expression checking invokes that provider with an expression payload selected from the source call
- **AND** it does not first type-check `example.answer` as an ordinary runtime function call

#### Scenario: Derive provider path resolves

- **WHEN** source uses `@derive(cli.Parser)` and the selected package profile registers `cli.Parser`
- **THEN** macro expansion invokes that provider with the target declaration metadata

#### Scenario: Expression provider path is unknown

- **WHEN** source uses `example.missing()` and no expression provider is registered for that path
- **THEN** expression checking reports an unresolved macro provider diagnostic
- **AND** it does not fall through to ordinary unresolved-name diagnostics for that macro-shaped call

#### Scenario: Derive provider path is unknown

- **WHEN** source uses `@derive(missing.Provider)` and no provider is registered for that path
- **THEN** macro expansion reports an unresolved macro provider diagnostic
- **AND** it does not treat the derive as an ordinary runtime call

### Requirement: Macro Provider Evaluation Protocol

cosmo0 SHALL evaluate macro invocations through an explicit provider protocol
instead of by executing arbitrary target program code.

#### Scenario: Expression provider receives bounded macro input

- **WHEN** expression checking invokes an expression macro provider
- **THEN** the provider input is a serialized macro function input record selected by the compiler, including provider identity, invocation identity, source span, C++ execution context, and an `Expr.Args` payload for the call arguments
- **AND** it does not include arbitrary runtime state or an executable target program handle

#### Scenario: Derive provider receives bounded macro input

- **WHEN** macro expansion invokes a derive provider
- **THEN** the provider input is a serialized macro function input record selected by the compiler, including target declaration facts, candidate attributes/defaults, expression fragments when applicable, and provider configuration
- **AND** it does not include arbitrary runtime state or an executable target program handle

#### Scenario: Provider returns bounded macro output

- **WHEN** a macro provider finishes evaluation
- **THEN** it returns generated artifacts, consumed attributes, diagnostics, and optional generated-source summary data
- **AND** later compiler phases accept only generated artifacts that pass ordinary validation

#### Scenario: Provider attempts target runtime execution

- **WHEN** a macro provider attempts to execute target program code during expansion
- **THEN** macro expansion rejects the operation with a macro diagnostic
- **AND** the package result does not depend on runtime execution

### Requirement: Macro Function Purity Contract

cosmo0 macro functions SHALL be specified as pure computations over the macro
input supplied by cosmo0.

#### Scenario: Compiler reruns a macro function

- **WHEN** cosmo0 evaluates the same macro function repeatedly with the same provider identity, serialized macro function input, C++ imports, provider source, target settings, and toolchain identity
- **THEN** each evaluation is required to produce the same generated artifacts, expression output, consumed attributes, diagnostics, and generated-source summary data
- **AND** cosmo0 may cache, discard, rerun, or compare macro function evaluations

#### Scenario: Macro function depends on hidden state

- **WHEN** a macro function observes ambient mutable state, time, randomness, IO, or any other hidden input and produces different results for the same cosmo0-provided input
- **THEN** the package has undefined behavior
- **AND** cosmo0 is not required to preserve that provider's observed evaluation order or number of executions

### Requirement: Generated Declaration Builder Output

cosmo0 macro providers SHALL produce generated declarations through a structured
generated declaration model rather than unvalidated source text.

#### Scenario: Provider returns generated declaration tree

- **WHEN** a macro provider generates a helper method
- **THEN** the provider output represents that method as a generated declaration tree with generated spans and origin spans
- **AND** cosmo0 validates the tree before integrating it into the package

#### Scenario: Derive provider returns implementation attachment

- **WHEN** a derive provider generates behavior for a target item in the first derive slice
- **THEN** the provider output represents that behavior as a generated trait implementation attachment
- **AND** cosmo0 validates the attachment before integrating it into the package
- **AND** the attachment does not introduce a new ordinary name-resolution binding

#### Scenario: Raw generated source text is rejected as primary output

- **WHEN** a macro provider returns only raw source text as generated code
- **THEN** macro expansion rejects the output or treats it as debug-only data
- **AND** the raw text does not bypass generated declaration validation

### Requirement: Generated Declaration Hygiene

cosmo0 SHALL distinguish public generated declarations from private generated
helpers and SHALL prevent generated names from silently capturing or shadowing
user declarations.

#### Scenario: Private helper gets a fresh name

- **WHEN** a macro provider requests a private helper declaration
- **THEN** cosmo0 assigns a stable fresh internal name that cannot be referenced accidentally by user source

#### Scenario: Public generated member collides with user member

- **WHEN** a macro provider generates a public member named `parse` and the user declaration already defines `parse` on the same type
- **THEN** expansion reports a duplicate generated declaration diagnostic
- **AND** the duplicate generated declaration is not accepted silently

### Requirement: Generated Diagnostics Preserve Origin

cosmo0 SHALL attach diagnostics from generated declarations to both the
generated span and the macro input span that caused the generated code.

#### Scenario: Generated type error points to macro input

- **WHEN** a generated declaration fails type checking because a reflected field type is not supported by the macro provider
- **THEN** the diagnostic identifies the generated declaration
- **AND** the diagnostic references the original field or derive attribute span that caused the generated code

### Requirement: Macro Expansion Safety Boundary

cosmo0 macro expansion SHALL be deterministic and SHALL NOT grant macro
providers direct compiler mutation or target package runtime execution.

#### Scenario: Provider attempts direct compiler mutation

- **WHEN** a macro provider attempts to patch typed modules, lowering IR, backend output, or compiler global state directly
- **THEN** macro expansion rejects the operation with a macro diagnostic
- **AND** the provider must affect the package only by returning serialized macro output

#### Scenario: Provider depends on ambient effects for output

- **WHEN** a macro provider uses filesystem contents, commands, environment state, network IO, time, randomness, or hidden mutable state to produce different macro output for the same cosmo0 input
- **THEN** package behavior is undefined
- **AND** cosmo0 is not required to preserve that provider's observed evaluation order or number of executions


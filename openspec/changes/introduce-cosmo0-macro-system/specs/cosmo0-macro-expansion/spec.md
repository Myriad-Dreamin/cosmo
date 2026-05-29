## ADDED Requirements

### Requirement: Deterministic Macro Expansion Phase

cosmo0 SHALL provide a deterministic macro expansion phase that turns accepted
macro decorators into ordinary generated declarations before type checking and
lowering depend on those declarations.

#### Scenario: Derive expands before generated method use

- **WHEN** a class annotated with `@derive(example.MakeParse)` generates a public static `parse` method
- **THEN** later source in the same package can resolve and type-check calls to that generated method
- **AND** the generated method enters ordinary lowering and backend emission

#### Scenario: Repeated expansion is stable

- **WHEN** the same package is checked twice with the same macro providers and source inputs
- **THEN** macro expansion produces the same generated declarations, diagnostics, and generated-source summary order

### Requirement: Macro Attribute Preservation

cosmo0 SHALL preserve structured macro attributes on supported declaration
shapes until the macro expansion phase consumes or rejects them.

#### Scenario: Field attribute reaches derive provider

- **WHEN** a class field is annotated with `@arg(long = "package", short = "p")`
- **THEN** the derive provider receives an attribute record containing the path `arg`, keyed string values `long` and `short`, and the field source span

#### Scenario: Malformed attribute is diagnosed

- **WHEN** a macro attribute uses an unsupported argument shape for the selected provider
- **THEN** expansion reports a macro diagnostic at the attribute span
- **AND** generated declarations for that macro invocation are not accepted

### Requirement: Macro Provider Registry

cosmo0 SHALL resolve macro provider paths through a deterministic provider
registry before invoking a derive or attribute macro.

#### Scenario: Provider path resolves

- **WHEN** source uses `@derive(cli.Parser)` and the selected package profile registers `cli.Parser`
- **THEN** macro expansion invokes that provider with the target declaration metadata

#### Scenario: Provider path is unknown

- **WHEN** source uses `@derive(missing.Provider)` and no provider is registered for that path
- **THEN** macro expansion reports an unresolved macro provider diagnostic
- **AND** it does not treat the derive as an ordinary runtime call

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
providers arbitrary filesystem, command, network, environment, or runtime
side effects.

#### Scenario: Provider requests an effect outside the boundary

- **WHEN** a macro provider attempts to read an undeclared file, run a command, inspect the environment, perform network IO, or execute runtime program code
- **THEN** macro expansion rejects the operation with a macro capability diagnostic
- **AND** the package result does not depend on that side effect

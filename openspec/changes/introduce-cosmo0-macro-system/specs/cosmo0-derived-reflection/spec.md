## ADDED Requirements

### Requirement: Derive Reflection Metadata

cosmo0 SHALL provide derive macro providers with compile-time reflection
metadata for the annotated declaration.

#### Scenario: Class metadata includes fields

- **WHEN** a derive provider is invoked for a class declaration
- **THEN** the provider input includes the class name, module path, visibility, type parameters admitted by the selected profile, field names, field types, field defaults, field attributes, doc comments, and source spans

#### Scenario: Sum metadata includes variants

- **WHEN** a derive provider is invoked for a sum type or class with variants
- **THEN** the provider input includes each variant name, payload shape, variant attributes, doc comments, and source spans

### Requirement: Derive Macro Contract

cosmo0 SHALL invoke `@derive(path)` providers with a stable metadata input and
SHALL accept only provider outputs that are valid generated declarations or
diagnostics.

#### Scenario: Provider generates static methods

- **WHEN** a derive provider returns a generated static method for the target type
- **THEN** cosmo0 attaches the method to the target type during generated declaration integration

#### Scenario: Provider output is invalid

- **WHEN** a derive provider returns malformed generated declarations or unsupported generated syntax
- **THEN** macro expansion reports an invalid provider output diagnostic
- **AND** the malformed output does not enter later compiler phases

#### Scenario: Provider cannot synthesize trusted typed expressions

- **WHEN** a derive provider emits generated code for the target type
- **THEN** it emits generated declarations and generated expressions that are checked later
- **AND** it cannot directly inject trusted typed expression artifacts into the checked program

### Requirement: Macro Attribute Consumption

cosmo0 SHALL track which attributes a derive provider consumes and SHALL reject
unconsumed macro attributes that remain on accepted source declarations.

#### Scenario: Derive consumes field argument attribute

- **WHEN** `@derive(cli.Parser)` consumes an `@arg(...)` attribute on a field
- **THEN** the field attribute is not reported as unknown after expansion

#### Scenario: Attribute typo is rejected

- **WHEN** a field uses `@agr(long = "package")` and no provider consumes `agr`
- **THEN** expansion reports an unconsumed macro attribute diagnostic at the attribute span

### Requirement: Reflection Metadata Is Compile-Time Only

cosmo0 SHALL make derive reflection metadata available to macro providers
without requiring runtime type metadata in compiled programs.

#### Scenario: Generated program does not carry reflection tables

- **WHEN** a package uses a derive macro that inspects field metadata at compile time
- **THEN** the emitted runtime program contains only the generated declarations needed by the package
- **AND** it does not include general-purpose reflection tables unless a separate runtime reflection capability is selected

### Requirement: Generated Source Inspection

cosmo0 SHALL provide deterministic inspection output for macro-generated
declarations for tests and debugging.

#### Scenario: Expansion summary lists generated declarations

- **WHEN** a package with derive macros is checked with generated-source inspection enabled
- **THEN** the output lists each macro invocation, consumed attributes, generated public declarations, generated private helpers, and originating source spans in deterministic order

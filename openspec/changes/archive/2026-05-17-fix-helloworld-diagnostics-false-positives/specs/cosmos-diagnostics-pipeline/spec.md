## ADDED Requirements

### Requirement: Compiler-Valid Samples Match Compiler Analysis

Cosmos document diagnostics SHALL analyze repository samples using compiler-compatible package, module, std, and prelude context so compiler-valid samples do not produce editor-only source diagnostics.

#### Scenario: HelloWorld sample is clean

- **WHEN** Cosmos analyzes `samples/HelloWorld/main.cos` using the sample package context
- **THEN** it returns an empty diagnostics array
- **AND** the VSCode-launched host publishes `textDocument/publishDiagnostics` for that URI with `diagnostics: []`

#### Scenario: Invalid edit still reports diagnostics

- **WHEN** the HelloWorld document text is changed to syntactically invalid Cosmo source
- **THEN** Cosmos publishes parser diagnostics for that edited snapshot
- **AND** the valid-sample regression path does not suppress diagnostics for the edited invalid text

### Requirement: Cosmo1 Checker Diagnostics Reflect Source Semantics

Cosmos diagnostics SHALL publish checker diagnostics produced by corrected Cosmo1 source semantics rather than by editor-side diagnostic suppression.

#### Scenario: Unit-returning bodies do not require explicit return

- **WHEN** a supported function body has no explicit `return` but is inferable as `Unit`
- **THEN** Cosmos does not publish `cosmo1.type.missing-return`

#### Scenario: Supported expressions are checked

- **WHEN** a compiler-supported expression construct appears in a valid sample
- **THEN** Cosmos checks the construct instead of publishing `cosmo1.type.unsupported-expr`

#### Scenario: Real type mismatch remains a source diagnostic

- **WHEN** a supported function declares an `i32` return type but returns `false`
- **THEN** Cosmos publishes a checker diagnostic for the type mismatch

### Requirement: Analyzer Infrastructure Is Not A Source Diagnostic

Cosmos diagnostics SHALL distinguish source diagnostics from analyzer infrastructure failures such as missing package root, module graph, std/prelude context, or package-surface context.

#### Scenario: Missing compiler context is fixed at construction

- **WHEN** editor analysis lacks context that the compiler-valid path provides
- **THEN** the diagnostics pipeline fixes package, module, std, or prelude context construction
- **AND** it does not hide source diagnostics by filtering diagnostic codes after they are produced

#### Scenario: Package graph failures remain attributed

- **WHEN** analysis discovers a real missing import or package graph failure
- **THEN** Cosmos publishes the diagnostic for the relevant URI and package root

# cosmos-diagnostics-pipeline Specification

## Purpose
TBD - created by archiving change add-cosmos-diagnostics-pipeline. Update Purpose after archive.
## Requirements
### Requirement: Document Diagnostics Analysis

`packages/cosmos` SHALL expose diagnostics entry points for open document snapshots that run parser diagnostics first and checker diagnostics only when parsing succeeds.

#### Scenario: Parser diagnostics take precedence

- **WHEN** an open document contains syntax that the `packages/cosmoc` parser rejects
- **THEN** Cosmos returns parser diagnostics for that document
- **AND** checker diagnostics are not produced for the same invalid parser result

#### Scenario: Checker diagnostics are returned after parse success

- **WHEN** an open document parses successfully but fails the expression checker
- **THEN** Cosmos returns checker diagnostics with stable code, message, severity, and range data

### Requirement: Package Diagnostics Analysis

`packages/cosmos` SHALL expose a package-level diagnostics entry point for module graph style package diagnostics.

#### Scenario: Missing import is reported

- **WHEN** a package module imports a module absent from the analyzed package set
- **THEN** Cosmos returns a package diagnostic with code `cosmo1.package.missing-import`

### Requirement: LSP Diagnostic Conversion

Cosmos SHALL convert compiler spans into zero-based LSP ranges and SHALL expose LSP-facing severity, code, source, and message data suitable for `textDocument/publishDiagnostics`.

#### Scenario: Span is converted to range

- **WHEN** a compiler diagnostic has a source span
- **THEN** Cosmos converts the span start and end offsets into zero-based line and character positions

#### Scenario: Diagnostic JSON is deterministic

- **WHEN** the same document or package diagnostics are converted repeatedly
- **THEN** Cosmos emits diagnostics in the same order and renders identical diagnostics JSON

### Requirement: Diagnostic Publication

Cosmos SHALL publish diagnostics through `ls-base` `publishDiagnostics` routing and clear stale diagnostics when a document closes or no diagnostics remain.

#### Scenario: Open or changed document publishes diagnostics

- **WHEN** Cosmos refreshes diagnostics for an open document snapshot
- **THEN** it sends a `textDocument/publishDiagnostics` notification for that URI

#### Scenario: Closed document clears diagnostics

- **WHEN** Cosmos refreshes diagnostics for a closed document snapshot
- **THEN** it sends a `textDocument/publishDiagnostics` notification with an empty diagnostics array for that URI

### Requirement: Diagnostic Publication Is URI-Scoped

Cosmos diagnostics publication SHALL be scoped to the target document URI so refresh, publish, and clear operations for one document cannot mutate diagnostics for a different open document.

#### Scenario: Refreshing one URI preserves another URI

- **WHEN** Cosmos has published diagnostics for document A and document B
- **AND** document A is refreshed
- **THEN** the refresh publishes diagnostics for document A's URI only
- **AND** document B's diagnostics are not cleared or replaced by document A's result

#### Scenario: Repeated unchanged refresh is stable

- **WHEN** Cosmos refreshes diagnostics for the same open document snapshot more than once
- **THEN** each refresh produces the same diagnostics JSON
- **AND** the result does not depend on whether the refresh was caused by open, change, active-editor switch, or save

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


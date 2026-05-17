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


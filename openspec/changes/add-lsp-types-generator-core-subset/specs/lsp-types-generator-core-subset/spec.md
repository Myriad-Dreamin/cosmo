# lsp-types-generator-core-subset Specification

## ADDED Requirements

### Requirement: Offline LSP Metamodel Input

The repository SHALL provide a checked-in LSP metamodel subset for deterministic
generation of the initial language-server protocol types.

#### Scenario: Metamodel subset is available without network access

- **WHEN** the LSP types generator package is inspected
- **THEN** the metamodel input is present under `packages/lsp-types/metamodel/`
- **AND** generator execution does not fetch protocol metadata from the network
- **AND** the checked-in input covers references, arrays, maps, unions, and literal object shapes used by the initial subset

### Requirement: Generated Core Protocol Types

The repository SHALL provide a generated Cosmo source module containing the
initial core LSP data structures and message shapes needed by early server
bootstrap work.

#### Scenario: Core data structures are generated

- **WHEN** `packages/lsp-types/src/lsp/core.cos` is inspected
- **THEN** it contains generated definitions for `Position`, `Range`, `Location`, `Diagnostic`, `MarkupContent`, and `Hover`
- **AND** generated field shapes preserve optional values, arrays, maps, references, unions, and string literal fields used by those definitions

#### Scenario: Session message types are generated

- **WHEN** `packages/lsp-types/src/lsp/core.cos` is inspected
- **THEN** it contains generated initialize, initialized, shutdown, and exit message shapes
- **AND** it contains generated core text-document notification shapes required by the initial language-server lifecycle

### Requirement: Deterministic Generator Validation

The repository SHALL expose validation for the LSP types generator that checks
both package compilation and deterministic regeneration.

#### Scenario: Generator output matches checked-in source

- **WHEN** the LSP types generator validation command is run
- **THEN** it executes `cosmo -p packages/lsp-types run`
- **AND** it fails if stdout differs from `packages/lsp-types/src/lsp/core.cos`

#### Scenario: Generator package compiles through Cosmo0

- **WHEN** the focused LSP types package test is run
- **THEN** the package loads successfully with the Cosmo stage1 profile
- **AND** compiling the runnable package emits host output containing the generated LSP core structures

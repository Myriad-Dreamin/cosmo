# lsp-types-generator-core-subset Specification

## Purpose
TBD - created by archiving change add-lsp-types-generator-core-subset. Update Purpose after archive.
## Requirements
### Requirement: Downloaded Full LSP Metamodel Input

The repository SHALL provide a `ureq-sys` backed command for downloading the
upstream full LSP metamodel used by the generator.

#### Scenario: Full metamodel is downloaded and ignored

- **WHEN** the LSP types generator package is inspected
- **THEN** a repository script downloads the full metamodel through `ureq-sys`
- **AND** the downloaded input path is `packages/lsp-types/metamodel/metaModel.json`
- **AND** that downloaded full metamodel is ignored by git

### Requirement: Generated Full Protocol Types

The repository SHALL provide checked-in generated Cosmo source modules for the
full LSP data structures, type aliases, enums, requests, and notifications.

#### Scenario: Full protocol modules are generated

- **WHEN** `packages/lsp-types/src/lsp/` is inspected
- **THEN** it contains `base.cos`, `type_aliases.cos`, `enums.cos`, `structs.cos`, `request.cos`, and `notification.cos`
- **AND** generated field shapes preserve optional values, arrays, maps, references, unions, null unions, and literal object shapes used by the full metamodel

#### Scenario: lspt-style protocol flavors are generated

- **WHEN** the generated full protocol modules are inspected
- **THEN** base output contains `Uri`, `HashMap`, and `UnionN` helper flavors
- **AND** request and notification output are split into dedicated modules with generated message classes and parameter/result aliases

### Requirement: Deterministic Generator Validation

The repository SHALL expose validation for the LSP types generator that checks
both package compilation and deterministic regeneration.

#### Scenario: Generator output matches checked-in source

- **WHEN** the LSP types generator validation command is run
- **THEN** it executes `cosmo -p packages/lsp-types run`
- **AND** it fails if generated file blocks differ from checked-in files under `packages/lsp-types/src/lsp/`

#### Scenario: Generator package compiles through Cosmo0

- **WHEN** the focused LSP types package test is run
- **THEN** the package loads successfully with the Cosmo stage1 profile
- **AND** compiling the runnable package emits host output containing the full metamodel generator

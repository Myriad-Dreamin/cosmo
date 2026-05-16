## ADDED Requirements

### Requirement: Cosmos Package Boundary

The repository SHALL provide `packages/cosmos` as the language-server package boundary for workspace roots, package sessions, document URI handling, and open-document snapshots.

#### Scenario: Cosmos package checks and compiles

- **WHEN** the cosmo0 package pipeline loads, checks, and compiles `packages/cosmos`
- **THEN** the package includes workspace, session, URI, document, and LSP document-event bridge modules
- **AND** the package compiles without diagnostics
- **AND** diagnostics, hover, semantic queries, and editor-client integration are not part of this package slice

### Requirement: Document URI Normalization

`packages/cosmos` SHALL normalize document URIs through `uri-sys` before storing or routing document state.

#### Scenario: File document URI becomes workspace location data

- **WHEN** a document URI is opened through the workspace
- **THEN** `packages/cosmos` parses and normalizes the URI through `uri-sys`
- **AND** it records both the normalized URI text and the local file path used for package selection

#### Scenario: URI failures are reported as workspace errors

- **WHEN** a document URI cannot be parsed or cannot be converted to a local file path
- **THEN** `packages/cosmos` returns a deterministic workspace error code and message

### Requirement: Open Document Snapshots

`packages/cosmos` SHALL store text-document snapshots for open, change, and close lifecycle operations.

#### Scenario: Open stores a snapshot

- **WHEN** a document is opened with URI, version, text, package root, and module path
- **THEN** the snapshot is stored by normalized URI
- **AND** the snapshot records that the document is open

#### Scenario: Change replaces an open snapshot

- **WHEN** a change is applied to an open document
- **THEN** the stored snapshot keeps the URI, file path, package root, and module path
- **AND** it replaces the version and text

#### Scenario: Close retains a closed snapshot

- **WHEN** a document is closed
- **THEN** the stored snapshot is retained with `is_open` set to false
- **AND** a later change for that closed snapshot is rejected

### Requirement: Package Root And Module Selection

`packages/cosmos` SHALL map local file documents to known package roots and package-relative module paths.

#### Scenario: Source document selects a package

- **WHEN** a document file path is under a registered package root's `src/` directory and ends in `.cos`
- **THEN** the workspace selects that package root
- **AND** the module path is the source-root-relative path without the `.cos` suffix

#### Scenario: Non-package document is rejected

- **WHEN** a document file path is not under a registered package root's `src/` directory
- **THEN** opening the document returns a workspace error instead of creating a snapshot

### Requirement: Package Analysis Session Cache

`packages/cosmos` SHALL cache one analysis-session placeholder per package root without running diagnostics or semantic queries in this slice.

#### Scenario: Session is reused

- **WHEN** the same package root requests a session more than once
- **THEN** the workspace returns the existing session identifier
- **AND** a different package root receives a distinct session identifier

### Requirement: LSP Document Event Bridge

`packages/cosmos` SHALL consume typed text-document events from `ls-base` without taking ownership of transport or editor-client integration.

#### Scenario: LSP event kind and URI are preserved

- **WHEN** a typed open, change, or close event is received from `ls-base`
- **THEN** `packages/cosmos` can convert it into a small event record containing the event kind and URI

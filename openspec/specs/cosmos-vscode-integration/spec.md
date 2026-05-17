# cosmos-vscode-integration Specification

## Purpose
Define how the VSCode extension launches and communicates with the Cosmos
language-server host so editor diagnostics and hover requests are served by the
`packages/cosmos` pipeline.
## Requirements
### Requirement: VSCode Launches The Cosmos Host

The VSCode extension SHALL launch a bundled `cosmos-lsp-host` executable that
boots the `packages/cosmos` language-server package before serving Cosmo
language requests.

#### Scenario: Extension activation starts the host

- **WHEN** a Cosmo document activates the extension
- **THEN** the extension probes and starts the bundled language server host over
  stdio
- **AND** the host validates that `packages/cosmos` can be loaded from the local
  checkout or packaged extension payload
- **AND** the extension does not call legacy `packages/cosmo` or `cosmo0`
  language-service APIs for diagnostics or hover

### Requirement: VSCode Document Lifecycle Uses Cosmos Analysis

The VSCode extension SHALL synchronize Cosmo documents with the host and surface
diagnostics and hover results from the integrated server path.

#### Scenario: Open and changed documents publish diagnostics

- **WHEN** a Cosmo document is opened or changed
- **THEN** the host analyzes the latest document text
- **AND** the extension publishes the resulting diagnostics for that document URI

#### Scenario: Hover requests return Cosmos markdown

- **WHEN** VSCode requests hover for a supported Cosmo identifier
- **THEN** the host returns a markdown hover payload with a range covering the
  identifier

### Requirement: VSCode Semantic Navigation Uses Cosmos

The VSCode integration SHALL route go-to-definition and find-references requests for Cosmo documents to the Cosmos language-server host.

#### Scenario: Initialize advertises navigation

- **WHEN** the VSCode-launched Cosmos host responds to `initialize`
- **THEN** the server capabilities include definition provider support
- **AND** the server capabilities include references provider support

#### Scenario: Definition request flows through host

- **WHEN** VSCode sends `textDocument/definition` for a supported Cosmo identifier
- **THEN** the host responds with the Cosmos definition location result

#### Scenario: References request flows through host

- **WHEN** VSCode sends `textDocument/references` for a supported Cosmo identifier
- **THEN** the host responds with the Cosmos references location result

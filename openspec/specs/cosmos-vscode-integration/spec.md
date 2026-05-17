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

### Requirement: Language Server Output Channel

The VSCode integration SHALL expose a human-readable output channel for Cosmos language-server diagnostics and stderr.

#### Scenario: Activation creates the output channel

- **WHEN** the Cosmo extension activates
- **THEN** it creates or reuses a VSCode output channel named for the Cosmo language server
- **AND** the language client is configured to use that channel for client/server diagnostic output

#### Scenario: Server stderr is visible

- **WHEN** the language-server process writes to stderr
- **THEN** the stderr text is appended to the Cosmo language-server output channel
- **AND** protocol stdout is not appended to the human-readable channel

#### Scenario: Startup metadata is visible

- **WHEN** VSCode launches the Cosmos language-server host
- **THEN** the host emits startup metadata to stderr
- **AND** the metadata is appended to the Cosmo language-server output channel
- **AND** the metadata includes the git revision used to build or run the host
- **AND** the metadata includes relevant version values for the host/server and VSCode extension when available
- **AND** the metadata includes resolved non-secret launch configuration such as host command or path, workspace or server root, transport, and configuration source
- **AND** protocol stdout remains reserved for JSON-RPC/LSP messages

#### Scenario: User can reveal the channel

- **WHEN** the user invokes the Cosmo command for language-server output
- **THEN** VSCode reveals the Cosmo language-server output channel

#### Scenario: Startup failure is recorded

- **WHEN** probing or starting the Cosmos host fails
- **THEN** the failure details are appended to the output channel
- **AND** the activation error shown to the user points them to that channel

## ADDED Requirements

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

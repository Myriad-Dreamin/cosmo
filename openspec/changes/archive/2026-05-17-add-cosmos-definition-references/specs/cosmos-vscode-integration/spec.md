## ADDED Requirements

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

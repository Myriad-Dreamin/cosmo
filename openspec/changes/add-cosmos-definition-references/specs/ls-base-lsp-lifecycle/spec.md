## MODIFIED Requirements

### Requirement: LSP Capability Declaration

`packages/ls-base` SHALL expose a deterministic initial server capability declaration for lifecycle initialization.

#### Scenario: Initial capability JSON is stable

- **WHEN** the initialize result is encoded
- **THEN** it includes text-document synchronization with open/close support
- **AND** it includes incremental text-document change support
- **AND** it includes hover, definition, and references provider fields
- **AND** its field ordering is deterministic

#### Scenario: Hover capability can be enabled

- **WHEN** hover support is enabled on the LSP server
- **THEN** the server records that hover is available in its capability declaration
- **AND** `textDocument/hover` is registered as a handled request method

#### Scenario: Definition capability can be enabled

- **WHEN** definition support is enabled on the LSP server
- **THEN** the server records that definition is available in its capability declaration
- **AND** `textDocument/definition` is registered as a handled request method

#### Scenario: References capability can be enabled

- **WHEN** references support is enabled on the LSP server
- **THEN** the server records that references are available in its capability declaration
- **AND** `textDocument/references` is registered as a handled request method

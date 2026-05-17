## MODIFIED Requirements

### Requirement: LSP Capability Declaration

`packages/ls-base` SHALL expose a deterministic initial server capability declaration for lifecycle initialization.

#### Scenario: Initial capability JSON is stable

- **WHEN** the initialize result is encoded
- **THEN** it includes text-document synchronization with open/close support
- **AND** it includes incremental text-document change support
- **AND** its field ordering is deterministic

#### Scenario: Hover capability can be enabled

- **WHEN** hover support is enabled on the LSP server
- **THEN** the server records that hover is available in its capability declaration
- **AND** `textDocument/hover` is registered as a handled request method

### Requirement: LSP Lifecycle Dispatch

`packages/ls-base` SHALL define transport-agnostic LSP lifecycle dispatch on top of the JSON-RPC session core.

#### Scenario: Successful response helper emits a JSON-RPC result

- **WHEN** a successful request response is emitted with a JSON result payload
- **THEN** the outbound session receives a JSON-RPC response for that request id
- **AND** the result payload is preserved as JSON data instead of a quoted string

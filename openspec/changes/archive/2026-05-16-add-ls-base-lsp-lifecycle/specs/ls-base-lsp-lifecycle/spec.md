## ADDED Requirements

### Requirement: LSP Lifecycle Dispatch

`packages/ls-base` SHALL define transport-agnostic LSP lifecycle dispatch on top of the JSON-RPC session core.

#### Scenario: Initialize and initialized transition the server

- **WHEN** the server receives an `initialize` request
- **THEN** it records that an initialize result was sent
- **AND** it emits a JSON-RPC response containing server capabilities
- **WHEN** the server receives an `initialized` notification
- **THEN** it transitions to ready state

#### Scenario: Shutdown and exit transition the server

- **WHEN** the server receives a `shutdown` request
- **THEN** it emits a JSON-RPC `null` result
- **AND** it transitions to shutdown state
- **WHEN** the server receives an `exit` notification
- **THEN** it transitions to exited state

### Requirement: LSP Capability Declaration

`packages/ls-base` SHALL expose a deterministic initial server capability declaration for lifecycle initialization.

#### Scenario: Initial capability JSON is stable

- **WHEN** the initialize result is encoded
- **THEN** it includes text-document synchronization with open/close support
- **AND** it includes incremental text-document change support
- **AND** its field ordering is deterministic

### Requirement: Text Document Notification Routing

`packages/ls-base` SHALL route core text-document notifications into typed document events without transport concerns.

#### Scenario: Open, change, and close notifications are recorded

- **WHEN** the server receives `textDocument/didOpen`, `textDocument/didChange`, and `textDocument/didClose`
- **THEN** it records typed document events with the text document URI

#### Scenario: Diagnostics publish as an outbound notification

- **WHEN** diagnostics are published for a document URI
- **THEN** the server emits a `textDocument/publishDiagnostics` notification through the JSON-RPC session

### Requirement: Extensible LSP Handler Registration

`packages/ls-base` SHALL allow request and notification methods to be registered independently from process and transport wiring.

#### Scenario: Handler registration is method based

- **WHEN** a request or notification method is registered
- **THEN** dispatch can recognize the method as handled
- **AND** duplicate registration does not add duplicate handler entries

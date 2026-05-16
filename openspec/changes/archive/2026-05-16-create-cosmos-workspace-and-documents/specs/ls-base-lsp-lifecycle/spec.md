## MODIFIED Requirements

### Requirement: Text Document Notification Routing

`packages/ls-base` SHALL route core text-document notifications into typed document events without transport concerns.

#### Scenario: Open, change, and close notifications are recorded

- **WHEN** the server receives `textDocument/didOpen`, `textDocument/didChange`, and `textDocument/didClose`
- **THEN** it records typed document events with the text document URI
- **AND** `packages/cosmos` can consume those typed events through its document-event bridge

#### Scenario: Diagnostics publish as an outbound notification

- **WHEN** diagnostics are published for a document URI
- **THEN** the server emits a `textDocument/publishDiagnostics` notification through the JSON-RPC session

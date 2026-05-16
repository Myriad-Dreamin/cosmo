## ADDED Requirements

### Requirement: JSON-RPC Envelope Models

`packages/ls-base` SHALL define transport-agnostic JSON-RPC 2.0 models for ids, requests, responses, notifications, and errors.

#### Scenario: Envelope kinds are represented

- **WHEN** `ls-base` is compiled
- **THEN** it provides request, response, notification, error, and id models
- **AND** ids support string, number-text, and null forms

#### Scenario: Malformed envelopes are deterministic

- **WHEN** inbound JSON cannot be parsed
- **THEN** decoding returns JSON-RPC parse error code `-32700`
- **AND** unsupported batch envelopes return invalid-request code `-32600`

### Requirement: JSON-RPC Envelope Codec

`packages/ls-base` SHALL encode and decode JSON-RPC 2.0 request, response, and notification envelopes deterministically.

#### Scenario: Outbound envelopes have stable field order

- **WHEN** requests, responses, or notifications are encoded
- **THEN** the emitted JSON begins with `jsonrpc`
- **AND** remaining envelope fields are emitted in deterministic order

#### Scenario: Inbound envelopes decode by shape

- **WHEN** an inbound envelope has a method and an id
- **THEN** it decodes as a request
- **WHEN** an inbound envelope has a method and no id
- **THEN** it decodes as a notification
- **WHEN** an inbound envelope has an id and exactly one of result or error
- **THEN** it decodes as a response

### Requirement: JSON-RPC Session Core

`packages/ls-base` SHALL provide an in-memory JSON-RPC session core that tracks pending outbound requests and correlates inbound responses without depending on stdio or editor process wiring.

#### Scenario: Pending requests are correlated

- **WHEN** a session sends two outbound requests
- **AND** receives a response for the first id
- **THEN** the first pending request is removed
- **AND** the later pending request remains tracked

#### Scenario: Dispatch state is in memory

- **WHEN** a session sends or receives JSON-RPC messages
- **THEN** outbound JSON envelopes and inbound decoded messages are retained in session memory

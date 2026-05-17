## ADDED Requirements

### Requirement: Position-Based Definition Lookup

`packages/cosmos` SHALL map an LSP text-document position in an open document snapshot to the definition location for supported Cosmo symbols.

#### Scenario: Reference resolves to declaration

- **WHEN** a definition request targets a supported local, parameter, function, member, or type reference
- **THEN** Cosmos returns an LSP location for the declaration that defines the target symbol
- **AND** the location URI is the analyzed document URI
- **AND** the range covers the declaration identifier

#### Scenario: Declaration resolves to itself

- **WHEN** a definition request targets the identifier at a supported declaration site
- **THEN** Cosmos returns that declaration's own LSP location

#### Scenario: Unsupported positions are empty

- **WHEN** a definition request targets whitespace, punctuation, a closed document snapshot, a parse-failed document, or a symbol kind not supported by the current analysis path
- **THEN** Cosmos returns an empty definition result

### Requirement: Position-Based References Lookup

`packages/cosmos` SHALL map an LSP text-document position in an open document snapshot to deterministic references for the same supported symbol within that snapshot.

#### Scenario: References include declaration when requested

- **WHEN** a references request targets a supported symbol
- **AND** the request context has `includeDeclaration` set to true
- **THEN** Cosmos returns the declaration location and all supported reference locations in deterministic source order

#### Scenario: References exclude declaration when requested

- **WHEN** a references request targets a supported symbol
- **AND** the request context has `includeDeclaration` set to false
- **THEN** Cosmos returns supported reference locations without the declaration location

#### Scenario: Unsupported references are empty

- **WHEN** a references request targets whitespace, punctuation, a closed document snapshot, a parse-failed document, or a symbol kind not supported by the current analysis path
- **THEN** Cosmos returns an empty references array

### Requirement: Navigation JSON Rendering

Cosmos SHALL render definition and references responses as deterministic LSP JSON using zero-based ranges.

#### Scenario: Location JSON is stable

- **WHEN** the same definition or references lookup is evaluated repeatedly against the same document snapshot
- **THEN** the generated JSON is byte-for-byte stable
- **AND** locations are ordered by URI, start line, start character, end line, and end character

### Requirement: Navigation Server Wiring

`packages/cosmos` SHALL expose definition and references response helpers that interoperate with the `ls-base` JSON-RPC server lifecycle.

#### Scenario: Navigation capabilities can be enabled

- **WHEN** Cosmos enables semantic navigation on an `ls-base` server
- **THEN** the server advertises definition and references support
- **AND** `textDocument/definition` and `textDocument/references` are registered as handled request methods

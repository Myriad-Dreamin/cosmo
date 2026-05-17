# cosmos-hover Specification

## Purpose
TBD - created by archiving change add-cosmos-hover. Update Purpose after archive.
## Requirements
### Requirement: Position-Based Hover Lookup
`packages/cosmos` SHALL map an LSP text-document position in an open document snapshot to the narrowest supported syntax or semantic target available from the existing parser, name-resolution, declaration-resolution, expression type-checking, and diagnostics analysis path.

#### Scenario: Supported identifiers produce hover
- **WHEN** a hover request targets a supported local, function, member, or type identifier
- **THEN** cosmos returns a hover result with deterministic content and a range covering the identifier

#### Scenario: Unsupported positions are empty
- **WHEN** a hover request targets whitespace, punctuation, a closed document snapshot, or a position without a supported syntax or semantic target
- **THEN** cosmos returns an empty hover result

### Requirement: Hover Rendering
Hover output SHALL render concise deterministic markup for declaration and inferred-type targets without exposing go-to-definition, references, completion, semantic-token, or editor-client behavior.

#### Scenario: Repeated rendering is stable
- **WHEN** the same supported hover lookup is evaluated repeatedly against the same document snapshot
- **THEN** the generated hover JSON is byte-for-byte stable

### Requirement: Hover Server Wiring
`packages/cosmos` SHALL expose hover request response helpers that interoperate with the `ls-base` JSON-RPC server lifecycle.

#### Scenario: Hover capability can be enabled
- **WHEN** cosmos enables hover on an `ls-base` server
- **THEN** the server advertises hover support and registers `textDocument/hover`


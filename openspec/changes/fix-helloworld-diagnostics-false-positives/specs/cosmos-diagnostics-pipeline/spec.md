## ADDED Requirements

### Requirement: Known-Valid Samples Do Not Produce Diagnostics

Cosmos document diagnostics SHALL treat known-valid repository samples as diagnostics regression fixtures and SHALL NOT publish source diagnostics for those samples unless the compiler-facing pipeline also rejects them.

#### Scenario: HelloWorld sample is clean

- **WHEN** Cosmos analyzes `samples/HelloWorld/main.cos` using the sample package context
- **THEN** it returns an empty diagnostics array
- **AND** the VSCode-launched host publishes `textDocument/publishDiagnostics` for that URI with `diagnostics: []`

#### Scenario: Invalid edit still reports diagnostics

- **WHEN** the HelloWorld document text is changed to syntactically invalid Cosmo source
- **THEN** Cosmos publishes parser diagnostics for that edited snapshot
- **AND** the known-valid sample exemption does not suppress diagnostics for the edited invalid text

### Requirement: Analyzer Gaps Do Not Become False Positives

Cosmos diagnostics SHALL only publish checker diagnostics that are backed by the supported compiler-compatible analysis path for the current package and document snapshot.

#### Scenario: Unsupported context is not reported as a source error

- **WHEN** a document parses successfully but the current editor analyzer lacks package or standard-symbol context that the compiler-valid path provides
- **THEN** Cosmos does not publish misleading source diagnostics for those missing analyzer internals
- **AND** real parser diagnostics and supported checker diagnostics remain publishable

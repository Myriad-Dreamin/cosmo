## 1. Cosmos Navigation

- [x] 1.1 Add position-based definition lookup for supported identifiers using the existing semantic analysis path.
- [x] 1.2 Add references lookup for supported identifiers in the current open document snapshot.
- [x] 1.3 Render deterministic LSP `Location` JSON with URI and zero-based ranges.
- [x] 1.4 Return empty results for whitespace, punctuation, closed documents, parse failures, and unsupported symbols.

## 2. LSP Lifecycle

- [x] 2.1 Extend `LspServerCapabilities` with definition and references provider flags.
- [x] 2.2 Add enable helpers that register `textDocument/definition` and `textDocument/references`.
- [x] 2.3 Update lifecycle tests for deterministic initialize JSON and handler registration.

## 3. VSCode Host Integration

- [x] 3.1 Advertise `definitionProvider` and `referencesProvider` in the VSCode-launched host initialize result.
- [x] 3.2 Dispatch `textDocument/definition` and `textDocument/references` through the host wrapper.
- [x] 3.3 Add smoke tests for definition and references requests against a document with local, parameter, function, member, and type references.

## 4. Validation

- [x] 4.1 Verify declaration-name lookup returns the declaration's own location.
- [x] 4.2 Verify reference-name lookup returns the target declaration location for definition requests.
- [x] 4.3 Verify references honor `includeDeclaration`.
- [x] 4.4 Verify repeated lookup returns byte-for-byte stable JSON ordering.

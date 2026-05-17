## 1. Client Refresh

- [x] 1.1 Add VSCode integration handling for active text editor changes when the active document is a Cosmo file.
- [x] 1.2 Request or republish diagnostics for the active document using its current in-memory text and URI.
- [x] 1.3 Ensure the refresh path does not require `Ctrl+S` and does not depend on the file's persisted on-disk contents.

## 2. Per-URI Server Semantics

- [x] 2.1 Verify diagnostics are stored, published, and cleared by normalized document URI.
- [x] 2.2 Ensure refreshing document A never clears or overwrites diagnostics for document B.
- [x] 2.3 Ensure an unchanged save produces the same diagnostics as the pre-save snapshot.

## 3. Validation

- [x] 3.1 Add a host/client smoke test that opens two Cosmo documents with different diagnostics states and switches the active editor.
- [x] 3.2 Verify diagnostics shown after the switch correspond to the active document URI.
- [x] 3.3 Verify pressing save without text changes does not make diagnostics disappear.

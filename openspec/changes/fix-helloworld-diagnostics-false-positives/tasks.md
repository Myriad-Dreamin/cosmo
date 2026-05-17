## 1. Reproduction

- [x] 1.1 Add a direct Cosmos diagnostics test for the exact text of `samples/HelloWorld/main.cos`.
- [x] 1.2 Add or update a VSCode host smoke test that opens the real HelloWorld sample URI and observes `textDocument/publishDiagnostics`.
- [x] 1.3 Capture the current false-positive diagnostic codes before changing behavior so the regression is explicit.

## 2. Diagnostics Fix

- [x] 2.1 Fix package root, module path, standard symbol, or checker-context handling so HelloWorld analysis matches the compiler-valid path.
- [x] 2.2 Ensure unsupported analyzer limitations for known-valid constructs do not become editor source diagnostics.
- [x] 2.3 Keep parser diagnostics taking precedence for syntactically invalid documents.
- [x] 2.4 Keep checker diagnostics for documents that parse successfully and genuinely fail supported checking.

## 3. Validation

- [x] 3.1 Verify `samples/HelloWorld/main.cos` publishes `[]` diagnostics through the host.
- [x] 3.2 Verify an intentionally invalid edit of the same document still publishes at least one diagnostic.
- [x] 3.3 Run the package and VSCode smoke tests that cover Cosmos diagnostics.

## 1. Package Layout

- [ ] 1.1 Create `packages/cosmos/` with initial workspace, session, and document modules.
- [ ] 1.2 Define the package layout consumed by later diagnostics and hover work.

## 2. URI and Document Model

- [ ] 2.1 Integrate `uri-sys` for document URI parsing and normalization.
- [ ] 2.2 Add open, change, and close snapshot storage for text documents.

## 3. Package Selection

- [ ] 3.1 Map documents to package roots and module paths.
- [ ] 3.2 Cache per-package analysis sessions without adding semantic features yet.

## 4. Validation

- [ ] 4.1 Add workspace fixture tests.
- [ ] 4.2 Add document lifecycle snapshot tests.
- [ ] 4.3 Add package-root and module-selection tests.

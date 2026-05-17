## 1. Position Lookup

- [x] 1.1 Map LSP positions to syntax or semantic nodes within an open document.
- [x] 1.2 Choose declaration or inferred-type targets for supported hover positions.

## 2. Rendering

- [x] 2.1 Render concise deterministic hover markup for declarations and inferred types.
- [x] 2.2 Return empty results for unsupported positions instead of partial placeholders.

## 3. Server Wiring

- [x] 3.1 Expose hover request handling through `packages/cosmos`.
- [x] 3.2 Return generated hover result types through `ls-base`.

## 4. Validation

- [x] 4.1 Add hover fixtures for locals, functions, members, and types.
- [x] 4.2 Add unsupported-position tests.
- [x] 4.3 Add repeated-run stability tests for rendered hover content.

## 1. Position Lookup

- [ ] 1.1 Map LSP positions to syntax or semantic nodes within an open document.
- [ ] 1.2 Choose declaration or inferred-type targets for supported hover positions.

## 2. Rendering

- [ ] 2.1 Render concise deterministic hover markup for declarations and inferred types.
- [ ] 2.2 Return empty results for unsupported positions instead of partial placeholders.

## 3. Server Wiring

- [ ] 3.1 Expose hover request handling through `packages/cosmos`.
- [ ] 3.2 Return generated hover result types through `ls-base`.

## 4. Validation

- [ ] 4.1 Add hover fixtures for locals, functions, members, and types.
- [ ] 4.2 Add unsupported-position tests.
- [ ] 4.3 Add repeated-run stability tests for rendered hover content.

## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/type.typ` with string, char, and byte text assumptions.
- [x] 1.2 Update `docs/cosmo0/std.typ` with the `core0.text` API.
- [x] 1.3 Update `docs/cosmo0/runtime.typ` with any backend or extern support behind text APIs.

## 2. Standard API

- [x] 2.1 Add text interface declarations or source modules for length and emptiness checks.
- [x] 2.2 Add slice or text view APIs needed by source spans.
- [x] 2.3 Add char and byte access APIs needed by lexing.
- [x] 2.4 Add builder-like construction APIs if needed by diagnostics.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend `source/source.cos`.
- [x] 3.2 Add or extend `source/source_map.cos`.
- [x] 3.3 Add text helper functions shared by lexing and diagnostics.

## 4. Tests

- [x] 4.1 Add cosmo0 tests for length, slice, char access, byte access, and builder-like operations.
- [x] 4.2 Add cosmo1 source text fixture tests.
- [x] 4.3 Add negative tests for unavailable `core0.text`.
- [x] 4.4 Add tests proving text APIs are surfaced through std, not new descriptor metadata.

## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/std.typ` with `core0.json` APIs.
- [x] 1.2 Update `docs/cosmo0/runtime.typ` with JSON parse runtime binding requirements.
- [x] 1.3 Document that `core0.json` is not required by Stage 1.

## 2. Standard API

- [x] 2.1 Add `JsonValue` standard declarations.
- [x] 2.2 Add `Json.parse` standard declaration and runtime binding.
- [x] 2.3 Add object field and array access APIs.
- [x] 2.4 Add string, bool, null, and numeric access APIs needed by selected fixtures.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend `syntax/json_loader.cos`.
- [x] 3.2 Add selected parser JSON fixture loading into syntax arenas.

## 4. Tests

- [x] 4.1 Add JSON parse and accessor tests.
- [x] 4.2 Add missing `core0.json` capability diagnostics.
- [x] 4.3 Add cosmo1 JSON AST loading validation tests.
- [x] 4.4 Add tests proving JSON is std/extern-backed, not a descriptor family.

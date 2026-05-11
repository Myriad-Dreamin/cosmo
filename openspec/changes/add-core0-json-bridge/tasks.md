## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/std.typ` with `core0.json` APIs.
- [ ] 1.2 Update `docs/cosmo0/runtime.typ` with JSON parse runtime binding requirements.
- [ ] 1.3 Document that `core0.json` is not required by Stage 1.

## 2. Standard API

- [ ] 2.1 Add `JsonValue` standard declarations.
- [ ] 2.2 Add `Json.parse` standard declaration and runtime binding.
- [ ] 2.3 Add object field and array access APIs.
- [ ] 2.4 Add string, bool, null, and numeric access APIs needed by selected fixtures.

## 3. Cosmo1 Components

- [ ] 3.1 Add or extend `syntax/json_loader.cos`.
- [ ] 3.2 Add selected parser JSON fixture loading into syntax arenas.

## 4. Tests

- [ ] 4.1 Add JSON parse and accessor tests.
- [ ] 4.2 Add missing `core0.json` capability diagnostics.
- [ ] 4.3 Add cosmo1 JSON AST loading validation tests.
- [ ] 4.4 Add tests proving JSON is std/extern-backed, not a descriptor family.

## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/std.typ` with `core0.text-output` APIs.
- [ ] 1.2 Update `docs/cosmo0/runtime.typ` with output runtime binding requirements.
- [ ] 1.3 Document that diagnostic formatting belongs to cosmo1, not std.

## 2. Standard API

- [ ] 2.1 Add minimal output sink declarations or source module.
- [ ] 2.2 Add backend or extern binding for output support.
- [ ] 2.3 Ensure output APIs do not become descriptor operations.

## 3. Cosmo1 Component

- [ ] 3.1 Add or extend `driver/diagnostic.cos` to render diagnostics to an output sink.
- [ ] 3.2 Keep diagnostic rendering deterministic.

## 4. Tests

- [ ] 4.1 Add output support-code or extern binding tests.
- [ ] 4.2 Add cosmo1 diagnostic render smoke tests.
- [ ] 4.3 Add deterministic output tests.
- [ ] 4.4 Add negative tests for missing `core0.text-output`.

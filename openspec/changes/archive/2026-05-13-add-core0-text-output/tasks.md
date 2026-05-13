## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/std.typ` with `core0.text-output` APIs.
- [x] 1.2 Update `docs/cosmo0/runtime.typ` with output runtime binding requirements.
- [x] 1.3 Document that diagnostic formatting belongs to cosmo1, not std.

## 2. Standard API

- [x] 2.1 Add minimal output sink declarations or source module.
- [x] 2.2 Add backend or extern binding for output support.
- [x] 2.3 Ensure output APIs do not become descriptor operations.

## 3. Cosmo1 Component

- [x] 3.1 Add or extend `driver/diagnostic.cos` to render diagnostics to an output sink.
- [x] 3.2 Keep diagnostic rendering deterministic.

## 4. Tests

- [x] 4.1 Add output support-code or extern binding tests.
- [x] 4.2 Add cosmo1 diagnostic render smoke tests.
- [x] 4.3 Add deterministic output tests.
- [x] 4.4 Add negative tests for missing `core0.text-output`.

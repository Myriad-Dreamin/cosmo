## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/runtime.typ` with trusted extern ABI hook rules.
- [x] 1.2 Update `docs/cosmo0/std.typ` with std-owned extern-backed APIs.
- [x] 1.3 Update `docs/cosmo0/package.typ` with runtime binding availability rules.

## 2. Implementation

- [x] 2.1 Add extern binding metadata for trusted core0/std declarations.
- [x] 2.2 Track backend runtime symbols, includes, or support libraries separately from descriptors.
- [x] 2.3 Add diagnostics for missing or unsupported extern runtime bindings.
- [x] 2.4 Implement fixed-arity direct C extern bindings from `@extern("c")` declarations.
- [x] 2.5 Implement file-level `@include-c(...);` directives for ordered C include emission.

## 3. Cosmo1 Component

- [x] 3.1 Add a small cosmo1 smoke component that calls an extern-backed std function.
- [x] 3.2 Keep the smoke component independent of filesystem and command APIs.

## 4. Tests

- [x] 4.1 Add extern binding lowering or backend requirement tests.
- [x] 4.2 Add missing runtime symbol diagnostics tests.
- [x] 4.3 Add cosmo1 validation for the extern-backed std smoke component.
- [x] 4.4 Add direct C extern binding lowering, backend, and diagnostic tests.
- [x] 4.5 Add file-level C include ordering and validation tests.

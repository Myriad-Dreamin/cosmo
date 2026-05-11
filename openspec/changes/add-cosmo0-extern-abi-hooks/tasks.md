## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/runtime.typ` with trusted extern ABI hook rules.
- [ ] 1.2 Update `docs/cosmo0/std.typ` with std-owned extern-backed APIs.
- [ ] 1.3 Update `docs/cosmo0/package.typ` with runtime binding availability rules.

## 2. Implementation

- [ ] 2.1 Add extern binding metadata for trusted core0/std declarations.
- [ ] 2.2 Track backend runtime symbols, includes, or support libraries separately from descriptors.
- [ ] 2.3 Add diagnostics for missing or unsupported extern runtime bindings.

## 3. Cosmo1 Component

- [ ] 3.1 Add a small cosmo1 smoke component that calls an extern-backed std function.
- [ ] 3.2 Keep the smoke component independent of filesystem and command APIs.

## 4. Tests

- [ ] 4.1 Add extern binding lowering or backend requirement tests.
- [ ] 4.2 Add missing runtime symbol diagnostics tests.
- [ ] 4.3 Add cosmo1 validation for the extern-backed std smoke component.

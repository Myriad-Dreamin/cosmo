## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/spec.typ` with the concrete Stage 1 profile.
- [ ] 1.2 Update `docs/cosmo0/package.typ` with Stage 1 package validation behavior.
- [ ] 1.3 Update `docs/cosmo0/testing.typ` with Stage 1 validation expectations.

## 2. Package And Source

- [ ] 2.1 Add Stage 1 package metadata and capability profile.
- [ ] 2.2 Add `source/source.cos`.
- [ ] 2.3 Add `source/span.cos`.
- [ ] 2.4 Add `source/source_map.cos`.
- [ ] 2.5 Add `driver/diagnostic.cos`.
- [ ] 2.6 Add `lex/token.cos`.
- [ ] 2.7 Add `lex/lexer.cos`.

## 3. Validation

- [ ] 3.1 Add `check-package` validation for Stage 1.
- [ ] 3.2 Add `compile-package` validation through the cosmo0 C++ backend.
- [ ] 3.3 Add deterministic output validation.

## 4. Negative Tests

- [ ] 4.1 Add negative fixtures for accidental user generics.
- [ ] 4.2 Add negative fixtures for host `Type`, reflection, and staging.
- [ ] 4.3 Add negative fixtures for closures and unsupported higher-order APIs.
- [ ] 4.4 Add negative fixtures for missing Stage 1 capabilities.

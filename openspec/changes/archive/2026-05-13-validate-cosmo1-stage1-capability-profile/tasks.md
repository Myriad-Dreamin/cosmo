## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/spec.typ` with the concrete Stage 1 profile.
- [x] 1.2 Update `docs/cosmo0/package.typ` with Stage 1 package validation behavior.
- [x] 1.3 Update `docs/cosmo0/testing.typ` with Stage 1 validation expectations.

## 2. Package And Source

- [x] 2.1 Add Stage 1 package metadata and capability profile.
- [x] 2.2 Add `source/source.cos`.
- [x] 2.3 Add `source/span.cos`.
- [x] 2.4 Add `source/source_map.cos`.
- [x] 2.5 Add `driver/diagnostic.cos`.
- [x] 2.6 Add `lex/token.cos`.
- [x] 2.7 Add `lex/lexer.cos`.

## 3. Validation

- [x] 3.1 Add `check-package` validation for Stage 1.
- [x] 3.2 Add `compile-package` validation through the cosmo0 C++ backend.
- [x] 3.3 Add deterministic output validation.

## 4. Negative Tests

- [x] 4.1 Add negative fixtures for accidental user generics.
- [x] 4.2 Add negative fixtures for host `Type`, reflection, and staging.
- [x] 4.3 Add negative fixtures for closures and unsupported higher-order APIs.
- [x] 4.4 Add negative fixtures for missing Stage 1 capabilities.

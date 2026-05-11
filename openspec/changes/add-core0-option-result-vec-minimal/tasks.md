## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/type.typ` with sealed standard type application rules for `Option`, `Result`, and `Vec`.
- [ ] 1.2 Update `docs/cosmo0/class.typ` with variant construction and matching rules required by `Option` and `Result`.
- [ ] 1.3 Update `docs/cosmo0/std.typ` with the minimal Stage 1 API surface.

## 2. Standard API

- [ ] 2.1 Add or align std declarations for `Option<T>`.
- [ ] 2.2 Add or align std declarations for `Result<T, E>`.
- [ ] 2.3 Add or align std declarations for `Vec<T>`.
- [ ] 2.4 Keep implementation-specific descriptor lowering hidden behind the std capability.

## 3. Cosmo1 Components

- [ ] 3.1 Add or extend `driver/diagnostic.cos` to store diagnostics in `Vec<Diagnostic>`.
- [ ] 3.2 Add or extend `lex/lexer.cos` to produce or buffer `Vec<Token>`.

## 4. Tests

- [ ] 4.1 Add `Vec` construction, push, get, len, and mutation tests.
- [ ] 4.2 Add `Option` and `Result` construction and match tests.
- [ ] 4.3 Add cosmo1 diagnostics-list and token-buffer validation tests.
- [ ] 4.4 Add negative tests for user-defined generics or unavailable `core0.option-result-vec`.

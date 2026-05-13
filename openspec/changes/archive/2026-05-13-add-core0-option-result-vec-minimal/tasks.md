## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/type.typ` with sealed standard type application rules for `Option`, `Result`, and `Vec`.
- [x] 1.2 Update `docs/cosmo0/class.typ` with variant construction and matching rules required by `Option` and `Result`.
- [x] 1.3 Update `docs/cosmo0/std.typ` with the minimal Stage 1 API surface.

## 2. Standard API

- [x] 2.1 Add or align std declarations for `Option<T>`.
- [x] 2.2 Add or align std declarations for `Result<T, E>`.
- [x] 2.3 Add or align std declarations for `Vec<T>`.
- [x] 2.4 Keep implementation-specific descriptor lowering hidden behind the std capability.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend `driver/diagnostic.cos` to store diagnostics in `Vec<Diagnostic>`.
- [x] 3.2 Add or extend `lex/lexer.cos` to produce or buffer `Vec<Token>`.

## 4. Tests

- [x] 4.1 Add `Vec` construction, push, get, len, and mutation tests.
- [x] 4.2 Add `Option` and `Result` construction and match tests.
- [x] 4.3 Add cosmo1 diagnostics-list and token-buffer validation tests.
- [x] 4.4 Add negative tests for user-defined generics or unavailable `core0.option-result-vec`.

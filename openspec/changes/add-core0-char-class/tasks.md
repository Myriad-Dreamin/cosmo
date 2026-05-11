## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/expr.typ` with character literal and comparison assumptions used by classification.
- [ ] 1.2 Update `docs/cosmo0/std.typ` with `core0.char-class` APIs and supported-character scope.

## 2. Standard API

- [ ] 2.1 Add `is_ascii_alpha` or equivalent helper.
- [ ] 2.2 Add `is_ascii_digit` or equivalent helper.
- [ ] 2.3 Add whitespace helper.
- [ ] 2.4 Add identifier-start and identifier-continue helpers.

## 3. Cosmo1 Component

- [ ] 3.1 Add or extend `lex/lexer.cos` identifier scanning.
- [ ] 3.2 Add or extend `lex/lexer.cos` number scanning.
- [ ] 3.3 Add or extend `lex/lexer.cos` whitespace and punctuation scanning.

## 4. Tests

- [ ] 4.1 Add character classification unit tests.
- [ ] 4.2 Add cosmo1 tokenization fixtures for identifiers, numbers, whitespace, and punctuation.
- [ ] 4.3 Add negative tests for unsupported non-ASCII behavior if out of scope.
- [ ] 4.4 Add missing `core0.char-class` capability diagnostics.

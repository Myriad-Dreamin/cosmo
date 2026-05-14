## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/expr.typ` with numeric literal preservation rules.
- [x] 1.2 Update `docs/cosmo0/std.typ` with later `BigInt` and `BigDecimal` capability placeholders.
- [x] 1.3 Document that Stage 1 does not require arbitrary-precision numeric support.

## 2. Literal Support

- [x] 2.1 Preserve raw numeric literal text in token or parser-facing data.
- [x] 2.2 Add primitive parse hooks only where precision is not lost.
- [x] 2.3 Keep `BigInt` and `BigDecimal` out of the descriptor registry.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend numeric token handling in `lex/token.cos` and `lex/lexer.cos`.
- [x] 3.2 Prepare parser literal AST data for later stages.

## 4. Tests

- [x] 4.1 Add large integer literal preservation tests.
- [x] 4.2 Add radix/separator preservation tests if supported.
- [x] 4.3 Add tests proving Stage 1 does not require `core0.big-number`.
- [x] 4.4 Add tests proving big-number support is std-based, not descriptor-based.

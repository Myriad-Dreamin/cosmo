## ADDED Requirements

### Requirement: Lossless Numeric Literal Text

cosmo0 SHALL preserve numeric literal source text in lexer and parser-facing data before primitive or arbitrary-precision conversion.

#### Scenario: Large integer literal text is preserved

- **WHEN** Stage 1 source contains an integer literal larger than primitive integer ranges
- **THEN** the lexer emits a numeric token whose text is exactly the original literal spelling
- **AND** tokenization does not require `core0.big-number`

#### Scenario: Radix prefixes and separators are preserved

- **WHEN** Stage 1 source contains numeric literals with radix prefixes or digit separators
- **THEN** the numeric token text includes the accepted prefix and separators exactly as written
- **AND** later parser-facing literal AST data can store that raw text without parsing it into a primitive value

#### Scenario: Primitive conversion is delayed until safe

- **WHEN** a later stage has an expected primitive numeric type
- **THEN** it may parse the preserved numeric text into that primitive type only with overflow and precision diagnostics
- **AND** Stage 1 does not lose information by parsing numeric text into host `i64`, `u64`, or `f64` values during lexing

### Requirement: Big Number Support Is A Standard Capability

Arbitrary-precision numeric support SHALL be exposed through the `core0.big-number` standard capability rather than primitive or runtime descriptor families.

#### Scenario: BigInt and BigDecimal are std-owned

- **WHEN** later source requires arbitrary-precision integer or decimal parsing
- **THEN** it depends on standard API types such as `BigInt` and `BigDecimal`
- **AND** descriptor metadata does not register `BigInt`, `BigDecimal`, or `core0.big-number` as descriptor families

#### Scenario: Stage 1 remains independent

- **WHEN** the `cosmo1.stage1` profile validates source loading, tokenization, lexer tests, and parser smoke tests
- **THEN** validation succeeds without `core0.big-number`

## ADDED Requirements

### Requirement: Member And Call Typing

cosmo1 SHALL type-check field access, field assignment, method calls, top-level calls, and receiver mutability for the parser self-compile subset.

#### Scenario: Parser member access is typed

- **WHEN** source accesses or assigns a parser class field
- **THEN** cosmo1 validates the receiver type, field existence, field type, and mutation permission

#### Scenario: Parser calls are typed

- **WHEN** source calls a method or top-level helper used by `parser.cos`
- **THEN** cosmo1 validates the callee, argument count, argument types, receiver type, receiver mutability, and return type

### Requirement: Parser String Intrinsics

cosmo1 SHALL provide type signatures for the `String` operations used by `parser.cos`.

#### Scenario: String intrinsic is typed

- **WHEN** source calls parser-required string operations such as `len`, `slice`, or `byte_at`
- **THEN** cosmo1 assigns the descriptor-defined parameter and return types without requiring full standard library compilation

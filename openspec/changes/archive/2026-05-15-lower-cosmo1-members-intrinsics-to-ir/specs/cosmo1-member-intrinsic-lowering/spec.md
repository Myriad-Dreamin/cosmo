## ADDED Requirements

### Requirement: Member Lowering

cosmo1 SHALL lower field access, field assignment, and resolved method calls from typed parser-source expressions into IR.

#### Scenario: Field operations lower

- **WHEN** typed parser-source expressions read or write class fields
- **THEN** cosmo1 emits IR field get or field set operations with verified receiver and value types

#### Scenario: Method calls lower

- **WHEN** typed parser-source expressions call resolved methods
- **THEN** cosmo1 emits direct IR calls with receiver passing and verified argument types

### Requirement: Intrinsic Lowering

cosmo1 SHALL lower parser-required `String`, primitive, and helper operations into IR.

#### Scenario: String operation lowers

- **WHEN** typed parser-source expressions call required `String` operations
- **THEN** cosmo1 emits verifier-accepted IR representing those operations for later C++ emission

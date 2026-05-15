# cosmo1-declaration-lowering Specification

## Purpose
TBD - created by archiving change lower-cosmo1-declarations-to-ir. Update Purpose after archive.
## Requirements
### Requirement: Declaration Lowering

cosmo1 SHALL lower typed parser-source declarations and signatures into cosmo1 IR declarations before expression body lowering.

#### Scenario: Parser classes lower to IR

- **WHEN** typed parser-source class declarations are lowered
- **THEN** IR type declarations contain class names, fields, field types, and stable declaration ids

#### Scenario: Parser functions lower to IR

- **WHEN** typed parser-source functions and methods are lowered
- **THEN** IR function declarations contain stable symbols, parameters, receiver metadata, return types, and owner information for methods


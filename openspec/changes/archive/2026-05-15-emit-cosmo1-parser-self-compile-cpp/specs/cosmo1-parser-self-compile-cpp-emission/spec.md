## ADDED Requirements

### Requirement: Parser IR Emits C++

cosmo1 SHALL emit deterministic C++ from verified parser IR for `packages/cosmoc/src/parser.cos`.

#### Scenario: Parser C++ is emitted

- **WHEN** verified parser IR is passed to the cosmo1 C++ emitter
- **THEN** the output contains C++ definitions for parser classes, fields, methods, top-level functions, references, primitive operations, string runtime calls, and control flow

#### Scenario: Emission is deterministic

- **WHEN** the same verified parser IR is emitted repeatedly
- **THEN** the generated C++ output is byte-for-byte stable

### Requirement: Parser Self-Compile Validation

cosmo1 SHALL validate the end-to-end parser self-compile path through generated C++ acceptance.

#### Scenario: Parser self-compile succeeds

- **WHEN** a cosmo0-compiled cosmo1 pipeline compiles `packages/cosmoc/src/parser.cos`
- **THEN** generated C++ is accepted by the configured C++ toolchain and repeated runs produce deterministic output

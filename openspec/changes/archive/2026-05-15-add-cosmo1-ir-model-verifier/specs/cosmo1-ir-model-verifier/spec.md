## ADDED Requirements

### Requirement: Cosmo1 IR Model

cosmo1 SHALL define an IR model with modules, declarations, functions, blocks, locals, values, operations, and terminators sufficient for parser self-compile lowering.

#### Scenario: Function IR is representable

- **WHEN** cosmo1 represents a parser-source function
- **THEN** the IR contains parameters, locals, ordered blocks, operations, and a terminator for each block

### Requirement: IR Verification

cosmo1 SHALL verify IR structure before C++ emission.

#### Scenario: Invalid IR is rejected

- **WHEN** IR has missing blocks, invalid local use, invalid call shape, or return type mismatch
- **THEN** the verifier reports diagnostics and prevents emission

#### Scenario: IR debug output is deterministic

- **WHEN** equivalent verified IR is rendered repeatedly
- **THEN** the debug output is stable for snapshot tests

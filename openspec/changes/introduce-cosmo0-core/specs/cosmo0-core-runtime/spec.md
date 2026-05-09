## ADDED Requirements

### Requirement: Core Scalar and Text Types

cosmo0 SHALL expose core scalar and text types required by compiler implementation code, including unit, booleans, signed and unsigned integers, pointer-sized integers, bytes, characters, owned strings, string slices, and byte buffers.

#### Scenario: Source offsets use pointer-sized integers

- **WHEN** cosmo0 source defines spans, token offsets, or vector lengths
- **THEN** it can use `usize` or equivalent pointer-sized integer types

#### Scenario: Source text is inspectable

- **WHEN** cosmo0 source implements lexing or diagnostics over source text
- **THEN** it can inspect string length, slice text, and read bytes or characters through descriptor-defined APIs

### Requirement: String Builder

cosmo0 SHALL expose a descriptor-backed string builder suitable for diagnostics, pretty-printing, and C++ code generation.

#### Scenario: Builder creates diagnostic text

- **WHEN** source appends strings, characters, integers, and newlines to a string builder
- **THEN** cosmo0 type-checks the operations and can lower them to the registered runtime implementation

#### Scenario: Builder produces String

- **WHEN** source finishes building text
- **THEN** it can convert the builder into an owned `String`

### Requirement: JSON Runtime Bridge

cosmo0 SHALL expose a descriptor-backed JSON value and parser bridge for bootstrap stages that consume parser JSON or package metadata.

#### Scenario: JSON object field is read

- **WHEN** source parses JSON and requests an object field by string key
- **THEN** cosmo0 type-checks the operation and returns a descriptor-defined optional or result value

#### Scenario: JSON array is iterated

- **WHEN** source reads a JSON array field for AST children
- **THEN** cosmo0 can type-check iteration or indexed access over the descriptor-defined JSON array representation

### Requirement: Filesystem and Path Runtime

cosmo0 SHALL expose descriptor-backed path and filesystem operations needed by package loading, source loading, generated output writing, and smoke tests.

#### Scenario: Source file is read

- **WHEN** source calls a descriptor-defined filesystem read operation with a path
- **THEN** cosmo0 assigns a `Result<String, IoError>` or equivalent fallible result type

#### Scenario: Generated file is written

- **WHEN** source calls a descriptor-defined filesystem write operation with a path and content string
- **THEN** cosmo0 type-checks the operation and lowers it to the runtime implementation

### Requirement: Command Runtime

cosmo0 SHALL expose descriptor-backed command execution for stages that invoke external build or run commands, while allowing earlier stages to omit this descriptor.

#### Scenario: Command execution is available when descriptor is enabled

- **WHEN** a stage requires running a C++ compiler, linker, or external parser bridge
- **THEN** cosmo0 can type-check and lower command construction and execution through the registered descriptor

#### Scenario: Earlier stage can compile without command runtime

- **WHEN** a stage only checks source or emits C++ text
- **THEN** cosmo0 does not require the command runtime descriptor to be implemented

### Requirement: Lossless Numeric Literal Representation

cosmo0 SHALL support lossless representation of numeric literals needed by cosmo1 parser and evaluator stages, either through raw literal text, `BigInt`, `BigDecimal`, or a staged combination of these.

#### Scenario: Integer literal text is preserved

- **WHEN** cosmo1-stage code parses an integer literal before arbitrary precision math is available
- **THEN** cosmo0 provides a way to preserve the original literal text or parsed value without precision loss

#### Scenario: BigInt descriptor is available when required

- **WHEN** a later cosmo1 stage evaluates arbitrary precision integer expressions
- **THEN** cosmo0 can expose a descriptor-backed `BigInt` implementation

### Requirement: Runtime Descriptor Staging

cosmo0 SHALL allow core0 runtime descriptors to be implemented and validated in stages aligned with cosmo1 bootstrap milestones.

#### Scenario: Stage-required descriptors are enforced

- **WHEN** a cosmo1 validation stage declares a required descriptor set
- **THEN** cosmo0 rejects the stage if any required runtime descriptor is unavailable

#### Scenario: Later descriptors do not block earlier stages

- **WHEN** a descriptor is only required by a later cosmo1 stage
- **THEN** missing implementation of that descriptor does not block earlier stage validation

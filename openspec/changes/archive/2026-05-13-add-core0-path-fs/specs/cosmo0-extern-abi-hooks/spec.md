## MODIFIED Requirements

### Requirement: Trusted Extern ABI Metadata

cosmo0 SHALL allow only trusted core0/std declarations to attach extern ABI metadata. Extern metadata SHALL name the ABI, target runtime symbol, and backend requirements without adding a descriptor family for the same runtime domain. C++ extern target symbols SHALL be represented as structured qualified symbols rather than arbitrary C++ call expressions.

#### Scenario: Trusted filesystem read lowers to extern metadata

- **WHEN** the trusted `core0.path-fs` declaration `Fs.read_to_string(path): Result<String, IoError>` has no cosmo0 body
- **THEN** lowering records an extern binding using `cosmo0.extern.v0`
- **AND** lowering emits a call to the trusted `cosmo0_runtime::read_file` filesystem runtime symbol instead of a `Filesystem` or `Path` descriptor operation

### Requirement: Backend Runtime Requirement Tracking

cosmo0 backends SHALL track runtime symbol, include, support-library, and descriptor requirements as backend requirements. Runtime symbol requirements SHALL remain separate from descriptor operations.

#### Scenario: Filesystem read records runtime requirements

- **WHEN** the C++ backend emits a call to the trusted `core0.path-fs` read binding
- **THEN** the backend records the required filesystem runtime symbol and include requirement
- **AND** descriptor requirements remain unchanged

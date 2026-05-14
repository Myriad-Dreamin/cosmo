## MODIFIED Requirements

### Requirement: Trusted Core0 Std Extern Bindings

cosmo0 SHALL allow only trusted core0/std declarations to attach extern ABI metadata. Extern metadata SHALL name the ABI, target runtime symbol, and backend requirements without adding a descriptor family for the same runtime domain. C++ extern target symbols SHALL be represented as structured qualified symbols rather than arbitrary C++ call expressions.

#### Scenario: Command run binding is trusted std

- **WHEN** the trusted `core0.command` declaration `command_run(command: &Command): Result[CommandResult, CommandError]` has no cosmo0 body
- **THEN** cosmo0 attaches `cosmo0.extern.v0` metadata for `::cosmo0_runtime::command_run`
- **AND** the binding is accepted only as repository-owned core0/std API metadata
- **AND** arbitrary packages cannot introduce new bodyless command execution declarations

#### Scenario: Command binding emits runtime requirements

- **WHEN** the C++ backend emits a call to the trusted `core0.command` run binding
- **THEN** it records the runtime symbol `cosmo0_runtime::command_run`
- **AND** it records the process runtime include and support requirements required by the selected backend

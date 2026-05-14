## ADDED Requirements

### Requirement: Structured Command Capability

cosmo0 SHALL expose `core0.command` as a source-facing standard capability for later-stage external process construction and execution.

#### Scenario: Command construction is structured

- **WHEN** source constructs a command with an executable path
- **THEN** it can append ordered string arguments
- **AND** it can attach environment key/value overrides
- **AND** it can set an optional working directory
- **AND** it does not construct the process through a shell command string

#### Scenario: Command execution returns captured process data

- **WHEN** source executes a command through the standard API
- **THEN** the result type is `Result[CommandResult, CommandError]`
- **AND** `CommandResult` exposes exit status, stdout, and stderr
- **AND** non-zero process exit is represented by `CommandResult` rather than by losing captured output

#### Scenario: Command execution is extern-backed

- **WHEN** source calls `Command.run()`
- **THEN** lowering records a trusted `cosmo0.extern.v0` call to `::cosmo0_runtime::command_run`
- **AND** backend requirements include the matching runtime symbol and process runtime support

### Requirement: Command APIs Remain Std-Owned

Command execution SHALL be surfaced through `core0.command` standard APIs and extern ABI metadata rather than new runtime descriptor families.

#### Scenario: Command helpers do not become descriptors

- **WHEN** descriptor metadata is inspected for process execution support
- **THEN** `Command`, `CommandResult`, `CommandError`, `Process`, and `Shell` are not registered descriptor families
- **AND** backend/runtime support is recorded as extern requirements behind the standard API boundary

### Requirement: Cosmo1 Link Command Component

cosmo1 SHALL provide a later-stage `link/command` source component that builds command plans over `core0.command`.

#### Scenario: Link command source builds compiler invocations

- **WHEN** the link command source creates a compiler or linker invocation
- **THEN** it uses `Command` arguments rather than shell text
- **AND** it can compose driver input paths into the command plan

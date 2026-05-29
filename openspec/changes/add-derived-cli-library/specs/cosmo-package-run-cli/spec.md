## MODIFIED Requirements

### Requirement: Runnable Package Entrypoint Resolution

cosmo0 package run SHALL resolve the runnable entrypoint by package convention.

#### Scenario: Top-level zero-argument main is runnable

- **WHEN** a selected package exposes a body-backed top-level zero-argument `main` function returning `Unit` or an integer
- **THEN** package run accepts that function as the runnable entrypoint
- **AND** package compilation emits an executable host wrapper for it

#### Scenario: Top-level argv main is runnable

- **WHEN** a selected package exposes a body-backed top-level `main(args: Vec[String])` function returning `Unit` or an integer
- **THEN** package run accepts that function as the runnable entrypoint
- **AND** package compilation emits an executable host wrapper that forwards process arguments to `args`

#### Scenario: Missing runnable entrypoint is diagnosed

- **WHEN** a selected package does not expose a top-level `main` function
- **THEN** package run fails before host execution
- **AND** it reports a missing runnable entrypoint diagnostic

#### Scenario: Invalid runnable entrypoint is diagnosed

- **WHEN** a selected package exposes `main` with unsupported parameters, no body, an extern binding, or an unsupported return type
- **THEN** package run fails before host execution
- **AND** it reports an invalid runnable entrypoint diagnostic

## ADDED Requirements

### Requirement: Package Run Command Surface

`cmd/cosmo` SHALL expose a package-aware run command using `cosmo -p <package> run`.

#### Scenario: Package run command selects a package root

- **WHEN** a user invokes `cosmo -p <package> run`
- **THEN** the host treats `<package>` as the selected package root
- **AND** it runs package-aware validation and compilation instead of file-level `run`

#### Scenario: Trailing arguments are forwarded

- **WHEN** arguments appear after `run`, optionally separated by `--`
- **THEN** the host forwards those arguments unchanged to the spawned executable

#### Scenario: Package executable working directory is stable

- **WHEN** the host spawns the compiled package executable
- **THEN** the executable runs with the selected package root as its working directory

### Requirement: Runnable Package Entrypoint Resolution

cosmo0 package run SHALL resolve the runnable entrypoint by package convention.

#### Scenario: Top-level main is runnable

- **WHEN** a selected package exposes a body-backed top-level zero-argument `main` function returning `Unit` or an integer
- **THEN** package run accepts that function as the runnable entrypoint
- **AND** package compilation emits an executable host wrapper for it

#### Scenario: Missing runnable entrypoint is diagnosed

- **WHEN** a selected package does not expose a top-level `main` function
- **THEN** package run fails before host execution
- **AND** it reports a missing runnable entrypoint diagnostic

#### Scenario: Invalid runnable entrypoint is diagnosed

- **WHEN** a selected package exposes `main` with parameters, no body, an extern binding, or an unsupported return type
- **THEN** package run fails before host execution
- **AND** it reports an invalid runnable entrypoint diagnostic

### Requirement: Package Run Host Execution

Package run SHALL compile and execute the selected runnable package through host tooling.

#### Scenario: Runnable package compiles and runs

- **WHEN** package run validates a runnable package
- **THEN** the host writes the generated C++ output under the selected package's `target/` directory
- **AND** it compiles that output with a C++17 compiler
- **AND** it spawns the resulting executable

#### Scenario: Compile and run failures are reported clearly

- **WHEN** package validation, C++ compilation, or executable execution fails
- **THEN** the host exits non-zero
- **AND** it reports diagnostics, a compile log path, or the process exit code as applicable

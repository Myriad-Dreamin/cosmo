## ADDED Requirements

### Requirement: CLI11 Support Boundary

Cosmo SHALL wrap CLI11 behind a private support boundary used by the derived CLI
library rather than exposing CLI11 classes or templates to ordinary Cosmo source.

#### Scenario: Derived parser calls support boundary

- **WHEN** generated CLI parser code needs backend parsing behavior
- **THEN** it passes the Cosmo-owned CLI schema and argument strings to the `cli11-sys` boundary
- **AND** ordinary user source does not import or instantiate CLI11 C++ types

#### Scenario: CLI11 API remains hidden

- **WHEN** an external Cosmo package imports the public CLI library
- **THEN** raw CLI11 wrapper declarations remain private implementation details
- **AND** the public API is expressed through Cosmo `CliSchema`, `CliMatches`, `CliError`, and derive-generated methods

### Requirement: CLI11 Schema And Match Exchange

The `cli11-sys` boundary SHALL consume a deterministic schema representation
and return deterministic structured matches, help/version actions, or errors.

#### Scenario: Schema parses valid args

- **WHEN** `cli11-sys` receives a schema containing a `--package <PATH>` option and args containing `--package samples`
- **THEN** it returns structured matches that bind the `package` field to `samples`

#### Scenario: Backend reports parse error

- **WHEN** CLI11 rejects the provided args because an option is unknown or a required value is missing
- **THEN** `cli11-sys` returns a structured parse error with a renderable message
- **AND** it does not terminate the process directly

### Requirement: C++ Exception Containment

The `cli11-sys` boundary SHALL catch CLI11 exceptions and convert them into
Cosmo result data.

#### Scenario: CLI11 throws parse exception

- **WHEN** CLI11 reports parse failure through an exception or backend-specific control flow
- **THEN** the wrapper catches it before crossing the support boundary
- **AND** returns a Cosmo `CliError` or help/version action

### Requirement: Deterministic CLI11 Dependency Staging

The repository SHALL stage or vendor the CLI11 dependency deterministically for
package build and test runs.

#### Scenario: Package requiring CLI backend builds

- **WHEN** a package using `@derive(cli.Parser)` is built through the cosmo0 C++ backend
- **THEN** the package build can locate the pinned CLI11 headers or support artifact
- **AND** repeated builds use the same CLI11 version and wrapper source

#### Scenario: Missing support artifact is diagnosed

- **WHEN** a package requires `cli11-sys` and the CLI11 support dependency is missing or incompatible
- **THEN** package build reports a support-library diagnostic instead of an unresolved C++ include or linker failure

### Requirement: Backend Independence Path

The public derived CLI schema SHALL remain independent enough that a later
generated or pure-Cosmo parser backend can replace CLI11 without changing user
source.

#### Scenario: Schema debug output is backend neutral

- **WHEN** a derived parser's schema is inspected in tests
- **THEN** the schema output uses Cosmo CLI concepts such as command, option, positional, subcommand, default, and value parser
- **AND** it does not contain CLI11-specific class names or option objects

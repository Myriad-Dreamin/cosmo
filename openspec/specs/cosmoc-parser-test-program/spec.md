# cosmoc-parser-test-program Specification

## Purpose
TBD - created by archiving change add-cosmoc-parser-test-program. Update Purpose after archive.
## Requirements
### Requirement: Parser Test Program Entry Point

The repository SHALL provide `packages/cosmoc/src/parser_test.cos` as an
executable parser test program with a `main` function.

#### Scenario: parser_test owns main

- **WHEN** the cosmoc parser test target is built
- **THEN** `parser_test.cos` provides the program entry point
- **AND** `parser.cos` remains a library source without a `main` function

### Requirement: Shared Parser Fixtures

The parser test program and Scala cosmo0 parser tests SHALL use a shared parser
fixture manifest.

#### Scenario: Scala and Cosmo fixtures match

- **WHEN** parser fixtures are added, removed, or renamed
- **THEN** both `parser_test.cos` and tests for `cosmo0.Parser.scala` discover
  the same fixture set from the shared manifest

#### Scenario: Fixture outcome is shared

- **WHEN** a fixture is marked as an accepted or rejected parser input in the
  manifest
- **THEN** both parser validation paths use that expected outcome

### Requirement: Parser Test Program Compilation

`parser_test.cos` SHALL compile as an executable through the cosmo0 C++ backend
after the parser library target is supported.

#### Scenario: parser_test compiles

- **WHEN** the cosmo0 backend compiles the parser library and parser test
  program targets
- **THEN** the configured C++ toolchain accepts the generated parser test
  executable output

### Requirement: Parser Test Program Execution

`parser_test.cos` SHALL run the shared parser fixtures and report success or
failure through its process exit status.

#### Scenario: Fixture run succeeds

- **WHEN** all shared parser fixtures produce their expected parser outcomes
- **THEN** the parser test program exits successfully

#### Scenario: Fixture run fails

- **WHEN** any shared parser fixture produces an unexpected parser outcome
- **THEN** the parser test program exits unsuccessfully and identifies the
  failing fixture


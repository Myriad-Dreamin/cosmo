## ADDED Requirements

### Requirement: Independent Scala Package

The repository SHALL provide an independent Scala.js package for the cosmo0
compiler path under `packages/cosmo0`.

#### Scenario: cosmo0 package namespace is separate

- **WHEN** cosmo0 public compiler API source files are compiled
- **THEN** they use the `cosmo0` Scala package namespace
- **AND** they do not add cosmo0 compiler facade or result API types to the
  existing `cosmo` namespace

#### Scenario: Parser syntax remains compatible

- **WHEN** parser-owned syntax node source files are compiled by the `cosmo0`
  project
- **THEN** the syntax node package remains available as `cosmo.syntax`
- **AND** existing full compiler code can continue to reference those node types

#### Scenario: cosmo0 has its own sbt project

- **WHEN** sbt loads the repository build
- **THEN** `cosmo0` is available as a project rooted at `packages/cosmo0`
- **AND** the existing `cosmo` project remains available through its current sbt
  target name
- **AND** the existing `cosmo` project depends on `cosmo0` for the shared parser
  boundary

### Requirement: Shared Parser Integration

cosmo0 SHALL expose and own the shared Cosmo parser used by both cosmo0 and the
existing full compiler path.

#### Scenario: Full compiler imports cosmo0 parser

- **WHEN** the existing full compiler parses source
- **THEN** it invokes the parser through `cosmo0.Parser`
- **AND** it receives the compatible `cosmo.syntax` AST

#### Scenario: Source text parses successfully

- **WHEN** a caller invokes the cosmo0 parse API with source text accepted by the
  shared parser
- **THEN** the result status is `Succeeded`
- **AND** the result contains a parsed module value
- **AND** no diagnostic is required

#### Scenario: Source text fails to parse

- **WHEN** a caller invokes the cosmo0 parse API with invalid source text
- **THEN** the result status is `Failed`
- **AND** the result contains a diagnostic for the parse phase
- **AND** the diagnostic includes source span information when an error offset is
  available

### Requirement: Structured Phase Results

cosmo0 SHALL use structured result and diagnostic types for public phase APIs.

#### Scenario: Result reports phase status

- **WHEN** a caller receives a parse, check, or compile result
- **THEN** the result identifies the phase that produced it
- **AND** the result status is one of `Succeeded`, `Pending`, `Unsupported`, or
  `Failed`
- **AND** diagnostics use structured severity, code, message, and optional source
  span fields

#### Scenario: Source positions are stable

- **WHEN** a diagnostic references a source span
- **THEN** the span includes the source file name
- **AND** start and end positions include source offset, 1-based line, and 1-based
  column values

### Requirement: Unimplemented Phase Behavior

cosmo0 SHALL return structured unavailable-phase results for phases that are not
implemented yet.

#### Scenario: Check phase is pending

- **WHEN** a caller invokes the cosmo0 check API on parseable source before the
  subset checker exists
- **THEN** the result status is `Pending`
- **AND** the result contains an informational diagnostic explaining that cosmo0
  subset checking is not implemented yet
- **AND** it does not throw an implementation placeholder exception

#### Scenario: Compile phase is unsupported

- **WHEN** a caller invokes the cosmo0 compile API on parseable source before the
  compile pipeline exists
- **THEN** the result status is `Unsupported`
- **AND** the result contains an informational diagnostic explaining that cosmo0
  compilation is not implemented yet
- **AND** it does not throw an implementation placeholder exception

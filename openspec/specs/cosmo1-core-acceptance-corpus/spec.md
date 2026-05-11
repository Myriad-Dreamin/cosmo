# cosmo1-core-acceptance-corpus Specification

## Purpose
TBD - created by archiving change add-cosmo1-core-acceptance-corpus. Update Purpose after archive.
## Requirements
### Requirement: Core Acceptance Corpus Location

The repository SHALL provide a cosmo1 core acceptance corpus under a cosmo0/cosmo1
validation fixture area.

#### Scenario: Corpus path is discoverable

- **WHEN** repository tests or future cosmo0 tooling need the corpus path
- **THEN** the corpus is available at
  `fixtures/cosmo0/cosmo1/core-acceptance/cosmo1_core_acceptance.cos`
- **AND** a manifest in the same directory lists the corpus path

### Requirement: Corpus Source Conventions

The corpus SHALL document the source conventions it uses for standard generic
application, references, mutability, and descriptor-backed standard types.

#### Scenario: Standard generic spelling is canonicalized

- **WHEN** the corpus references descriptor-backed standard generic types
- **THEN** it uses `Name[Arg]` syntax such as `Vec[Token]`, `Option[Span]`,
  `Result[ExprId, Diagnostic]`, `Arena[Expr]`, `Id[Expr]`, `Map[Symbol, DefId]`,
  and `Set[Symbol]`

#### Scenario: References and mutability are represented

- **WHEN** the corpus declares receiver or parameter references
- **THEN** it uses `&self`, `&mut self`, `&T`, and `&mut T`
- **AND** mutable state is represented with `var` fields or locals and assignment

### Requirement: Compiler-Shaped Data Coverage

The corpus SHALL include representative cosmo1 compiler data declarations without
implementing compiler behavior.

#### Scenario: Core compiler records are present

- **WHEN** a later cosmo0 phase consumes the corpus
- **THEN** it encounters source files, spans, diagnostics, symbols, tokens, AST
  nodes, arenas, typed identifiers, scopes, maps, sets, and parser state

#### Scenario: Standard containers are exercised

- **WHEN** a later cosmo0 phase consumes the corpus
- **THEN** it encounters relevant uses of `Vec`, `Option`, `Result`, `Arena`,
  `Id`, `Map`, and `Set`

### Requirement: Expression Form Coverage

The corpus SHALL exercise the ordinary expression forms needed by compiler-shaped
cosmo1 code.

#### Scenario: Core expression forms are present

- **WHEN** a later cosmo0 phase consumes the corpus
- **THEN** it encounters methods, receiver references, mutation, field access,
  calls, assignments, conditionals, loops, variant construction, and `match`

### Requirement: Corpus Is Not a Full Compiler

The corpus SHALL remain an acceptance target rather than a working cosmo1
implementation.

#### Scenario: Full-language features are excluded

- **WHEN** the corpus source is reviewed or consumed by future cosmo0 tooling
- **THEN** it does not declare user-defined generics, traits, reflection, staging,
  closures, or full cosmo1 implementation logic

#### Scenario: Current validation remains lightweight

- **WHEN** the current repository test suite validates the corpus
- **THEN** it verifies manifest discoverability and shared-parser acceptance
- **AND** it does not require cosmo0 typing, LIR lowering, backend compilation, or
  package validation until those phases exist


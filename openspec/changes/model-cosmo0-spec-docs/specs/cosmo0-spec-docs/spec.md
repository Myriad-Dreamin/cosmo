## ADDED Requirements

### Requirement: cosmo0 Spec Document Skeleton

The repository SHALL maintain a `docs/cosmo0/` specification skeleton covering the cosmo0 subset boundary, type system, declarations, expressions, control flow, standard APIs, runtime hooks, package behavior, and testing policy.

#### Scenario: Required skeleton files exist

- **WHEN** the cosmo0 docs skeleton validation runs
- **THEN** `docs/cosmo0/spec.typ`, `type.typ`, `class.typ`, `expr.typ`, `control-flow.typ`, `std.typ`, `runtime.typ`, `package.typ`, and `testing.typ` exist

#### Scenario: Ownership is reviewable

- **WHEN** a future cosmo0 change alters source-facing behavior
- **THEN** reviewers can identify the owning `docs/cosmo0/` file for the affected behavior area

#### Scenario: Boundary examples are present

- **WHEN** reviewers inspect the cosmo0 docs skeleton
- **THEN** each required `docs/cosmo0/` file includes examples that illustrate accepted, rejected, or placeholder behavior shapes

### Requirement: cosmo0 Bug Spec Synchronization

The cosmo0 docs SHALL define a bug/spec sync policy requiring behavior-changing fixes to update regression tests and, when intended semantics change, the owning spec file.

#### Scenario: Bug fix changes intended behavior

- **WHEN** a compiler bug fix changes intended cosmo0 behavior
- **THEN** the same change updates the owning `docs/cosmo0/` file and adds regression coverage

#### Scenario: Bug fix restores documented behavior

- **WHEN** a compiler bug fix restores behavior already documented by `docs/cosmo0/`
- **THEN** the same change adds regression coverage without treating the previous implementation behavior as authoritative

### Requirement: Stage 1 Capability Profile Placeholder

The cosmo0 docs SHALL include a Stage 1 capability profile placeholder for later source loading, diagnostics, token, lexing, standard API, runtime, and package validation requirements.

#### Scenario: Stage 1 validation is cross-referenced

- **WHEN** reviewers inspect the Stage 1 placeholder
- **THEN** it references the OpenSpec change `validate-cosmo1-stage1-through-cosmo0`

#### Scenario: Stage 1 capability ownership is clear

- **WHEN** a later proposal fills a Stage 1 capability
- **THEN** the proposal can update the relevant owner files under `docs/cosmo0/`

### Requirement: Staged Runtime Spec Impact Validation

The repository SHALL provide lightweight validation or review guidance proving staged runtime proposals reference their cosmo0 spec impact.

#### Scenario: Skeleton validation checks docs and staged proposals

- **WHEN** the lightweight cosmo0 docs validation runs
- **THEN** it checks that required docs exist and that active staged runtime proposals reference changed `docs/cosmo0/` files or justify implementation-only status

#### Scenario: Descriptor or std proposal is reviewed

- **WHEN** a future descriptor/std proposal changes source-facing behavior
- **THEN** the proposal names the changed `docs/cosmo0/` files before relying on that behavior from cosmo1 source

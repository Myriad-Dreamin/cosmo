## MODIFIED Requirements

### Requirement: cosmo0 Spec Document Skeleton

The repository SHALL maintain a `docs/cosmo/` specification skeleton covering
the cosmo0 subset boundary, type system, declarations, expressions, control
flow, standard APIs, runtime hooks, package behavior, macro and reflection
behavior, and testing policy.

#### Scenario: Required skeleton files exist

- **WHEN** the cosmo0 docs skeleton validation runs
- **THEN** `docs/cosmo/spec.typ`, `type.typ`, `class.typ`, `expr.typ`, `control-flow.typ`, `std.typ`, `runtime.typ`, `package.typ`, `macro-expr.typ`, `compile-time-evaluation.typ`, and `testing.typ` exist

#### Scenario: Ownership is reviewable

- **WHEN** a future cosmo0 change alters source-facing behavior
- **THEN** reviewers can identify the owning `docs/cosmo/` file for the affected behavior area

#### Scenario: Boundary examples are present

- **WHEN** reviewers inspect the cosmo0 docs skeleton
- **THEN** each required `docs/cosmo/` file includes examples that illustrate accepted, rejected, or placeholder behavior shapes

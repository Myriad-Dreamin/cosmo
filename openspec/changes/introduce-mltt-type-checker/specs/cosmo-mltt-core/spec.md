## ADDED Requirements

### Requirement: MLTT Core Profile
Cosmo SHALL provide an experimental `mltt.core` checker profile for a small
Martin-Lof type theory core. The profile SHALL be separate from existing
parser-subset type checking and SHALL be allowed to reject full Cosmo features
that are outside the MLTT core slice.

#### Scenario: MLTT profile is selected
- **WHEN** a fixture or package-check experiment selects `mltt.core`
- **THEN** type checking uses the MLTT core profile
- **AND** the result identifies the `mltt.core` checker profile

#### Scenario: Unsupported full-language feature is rejected
- **WHEN** `mltt.core` receives source that uses traits, async, object dispatch, C++ imports, macros, reflection, or staging before those features are admitted
- **THEN** the checker reports an unsupported-feature diagnostic
- **AND** the checker does not produce MLTT core artifacts for the rejected construct

### Requirement: Core Term And Type Model
The `mltt.core` profile SHALL represent universes, dependent functions,
dependent pairs, variables, lambdas, applications, lets, equality, and
inductive-family references as core data.

#### Scenario: Dependent function is represented
- **WHEN** the profile elaborates a dependent function type
- **THEN** the core artifact represents a Pi binder with a domain and a body type scoped over the binder

#### Scenario: Equality type is represented
- **WHEN** the profile elaborates a propositional equality type
- **THEN** the core artifact records the compared type, left term, and right term

### Requirement: Pure Type-Level Computation Boundary
The MLTT core SHALL only reduce pure, total, transparent definitions during
definitional equality.

#### Scenario: Effectful term is excluded from conversion
- **WHEN** a term with `async`, `yield`, `throw`, mutation, IO, or another unhandled effect would be needed for type-level reduction
- **THEN** conversion rejects or postpones the reduction with a diagnostic or constraint
- **AND** the checker does not execute the effectful computation to decide type equality

#### Scenario: Transparent pure let can reduce
- **WHEN** conversion compares a transparent pure let-bound expression with its expanded form
- **THEN** the checker may reduce the let during definitional equality

### Requirement: First-Stage Normalization Boundary
The first MLTT checker profile SHALL declare the normalization strategy used by
conversion and SHALL NOT claim conformance to any stronger normalization strategy
that has not been separately specified and implemented.

#### Scenario: First-stage strategy is declared
- **WHEN** `mltt.core` performs conversion in the first implementation stage
- **THEN** the result identifies the first-stage normalization strategy such as `mltt.whnf-conversion`
- **AND** the checker does not claim support for an unspecified stronger strategy

#### Scenario: Stronger normalization profile is requested too early
- **WHEN** a fixture requests a normalization profile that is not implemented by `mltt.core`
- **THEN** the checker reports an unsupported-normalization-profile diagnostic
- **AND** it does not silently fall back while claiming conformance to the requested strategy

### Requirement: Inductive Family Metadata
The MLTT profile SHALL represent inductive families through declaration
metadata rather than hard-coded checker branches.

#### Scenario: Constructor result records indices
- **WHEN** a constructor declaration is loaded for an indexed family
- **THEN** the declaration metadata records the constructor telescope and the family indices in the constructor result type

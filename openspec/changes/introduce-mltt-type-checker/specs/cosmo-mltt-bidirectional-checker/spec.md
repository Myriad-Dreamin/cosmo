## ADDED Requirements

### Requirement: Bidirectional Checking
The MLTT checker SHALL expose separate check and infer modes.

#### Scenario: Lambda checks against Pi
- **WHEN** a lambda expression is checked against a Pi type
- **THEN** the checker extends the context with the Pi domain
- **AND** checks the lambda body against the Pi body specialized by the parameter

#### Scenario: Application infers through Pi
- **WHEN** an application is inferred and the function infers a Pi type
- **THEN** the checker checks the argument against the Pi domain
- **AND** returns the Pi body specialized by the argument as the application type

### Requirement: Conversion
The MLTT checker SHALL use definitional equality when checking whether an
inferred type satisfies an expected type. The first implementation SHALL perform
conversion through a declared first-stage normalization strategy rather than
through an unspecified evaluator.

#### Scenario: Type mismatch after normalization
- **WHEN** an inferred type and expected type are not definitionally equal after allowed normalization
- **THEN** the checker reports a type mismatch diagnostic with deterministic display text

#### Scenario: Beta-equivalent function types match
- **WHEN** two types differ only by allowed beta reduction or transparent let expansion
- **THEN** conversion accepts them as definitionally equal

#### Scenario: Conversion result records strategy
- **WHEN** conversion succeeds or fails in the first MLTT checker
- **THEN** the checker can report the normalization strategy used for that conversion
- **AND** the strategy is not reported as any stronger strategy unless that strategy is selected and implemented

### Requirement: Conservative Metavariables
The MLTT checker SHALL allow metavariables only as local elaboration aids and
SHALL report postponed or unsolved constraints deterministically.

#### Scenario: Metavariable remains unsolved
- **WHEN** checking completes with an unsolved metavariable that affects a public typed artifact
- **THEN** the checker reports an unsolved-constraint diagnostic

#### Scenario: Higher-order unification is outside the first profile
- **WHEN** solving a metavariable would require arbitrary higher-order unification
- **THEN** the checker rejects or postpones the constraint instead of guessing a solution

### Requirement: Deterministic Diagnostics
MLTT checker diagnostics SHALL include profile id, source span when available,
core context summary, and deterministic expected/actual display where applicable.

#### Scenario: Unknown variable is reported
- **WHEN** infer mode sees a variable not present in the MLTT context
- **THEN** the checker reports an unknown-variable diagnostic
- **AND** the diagnostic identifies the `mltt.core` profile

### Requirement: cosmo0 Experimental Hosting
MLTT checker experiments SHALL be permitted in Scala-side cosmo0 infrastructure
only when they are selected by checker profile metadata and kept out of the
default cosmo0 source subset.

#### Scenario: cosmo0 experiment identifies profile
- **WHEN** a cosmo0-side MLTT experiment checks or rejects an input
- **THEN** its result identifies the selected checker profile
- **AND** unsupported features are reported as ordinary diagnostics

#### Scenario: cosmo0 default subset remains stable
- **WHEN** the default `cosmo0.subset` package checker runs ordinary bootstrap source
- **THEN** MLTT-only features are not enabled implicitly by the presence of a cosmo0-side experiment

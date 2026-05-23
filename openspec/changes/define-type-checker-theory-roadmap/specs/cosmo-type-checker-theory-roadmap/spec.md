## ADDED Requirements

### Requirement: Theory Profile Registry
Cosmo SHALL maintain a registry of type-checker theory profiles. Each profile
SHALL identify its formal model, supported judgments, supported features,
unsupported features, soundness invariants, performance envelope, host/value-model
assumptions when applicable, and conformance suite groups.

#### Scenario: Profile declares scope and limits
- **WHEN** a theory profile is registered
- **THEN** the profile records the features it supports
- **AND** the profile records the features it deliberately rejects
- **AND** the profile records any invariants required for soundness

#### Scenario: Partial theory is valid when declared
- **WHEN** a checker implements only a documented subset of Cosmo's final type-system goals
- **THEN** the checker may still participate in conformance tests for that subset
- **AND** unsupported features are reported as unsupported rather than as internal errors

#### Scenario: cosmo0 reference implementation is profile-gated
- **WHEN** a theory profile has a Scala-side implementation under `packages/cosmo0`
- **THEN** the implementation identifies the same profile id and supported goals as the registry entry
- **AND** it remains behind explicit profile selection until it reaches the required integration level

### Requirement: Common Checking Goals
Cosmo SHALL define common checking goals that every checker profile either
implements or explicitly rejects.

#### Scenario: Common goal is unsupported
- **WHEN** a checker profile receives a common goal outside its declared scope
- **THEN** the checker result reports an unsupported-goal or unsupported-feature diagnostic
- **AND** the result identifies the checker profile that rejected the goal

#### Scenario: Artifact summary is deterministic
- **WHEN** a checker profile runs the same supported goal twice without source changes
- **THEN** the checker produces the same artifact summary and diagnostic summary

### Requirement: Normalization Strategy Profiles
Cosmo SHALL model normalization strategies as separate profiles used by
conversion rather than as implicit properties of all MLTT checkers.

#### Scenario: First MLTT conversion uses a narrow normalization strategy
- **WHEN** the first MLTT checker performs conversion
- **THEN** it uses a declared first-stage normalization strategy such as `mltt.whnf-conversion`
- **AND** it does not claim conformance to any unspecified stronger strategy

#### Scenario: Future normalization strategy declares its model
- **WHEN** a future normalization strategy profile is registered
- **THEN** the profile declares the formal and implementation model it depends on
- **AND** the profile declares host-language assumptions needed to implement that model

### Requirement: Conformance Levels
Cosmo SHALL classify checker profiles by conformance level before integrating
them into package compilation or editor services.

#### Scenario: Profile reaches goal-tested level
- **WHEN** a checker profile passes focused goal tests for its declared feature set
- **THEN** the profile may be marked goal-tested
- **AND** the profile is not automatically eligible for package-level compiler integration

#### Scenario: Profile reaches compiler-candidate level
- **WHEN** a checker profile has deterministic diagnostics, deterministic artifacts, integration fixture coverage, and a documented performance envelope
- **THEN** the profile may be considered a compiler-candidate

### Requirement: Performance And Failure Modes
Theory profiles SHALL document performance expectations and deterministic failure
modes for expensive operations such as normalization, conversion, trait solving,
coverage, and object-safety checks.

#### Scenario: Normalization exceeds its limit
- **WHEN** a normalization strategy exceeds its declared fuel, recursion, or cache limit
- **THEN** the checker reports a deterministic normalization-limit diagnostic
- **AND** the checker does not crash or produce nondeterministic typed artifacts

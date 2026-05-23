## ADDED Requirements

### Requirement: Profile-Gated Dependent Patterns
Dependent pattern matching SHALL be available only in checker profiles that
declare support for dependent-pattern elaboration.

#### Scenario: Unsupported profile rejects dependent pattern
- **WHEN** a checker profile that does not support dependent patterns receives a dependent pattern match
- **THEN** the checker reports an unsupported-dependent-pattern diagnostic
- **AND** the rejection does not imply that the source is invalid for profiles that support dependent patterns

#### Scenario: Supported profile elaborates dependent pattern
- **WHEN** a checker profile with dependent-pattern support receives an accepted dependent pattern match
- **THEN** the checker elaborates the match through constructor metadata and index refinement

### Requirement: Constructor Refinement
Dependent pattern elaboration SHALL refine branch contexts by unifying the
scrutinee family type with the matched constructor result type.

#### Scenario: Impossible constructor is detected
- **WHEN** a pattern attempts to match a constructor whose result indices cannot unify with the scrutinee type
- **THEN** the branch is marked impossible or rejected as unreachable according to the profile's accepted syntax

#### Scenario: Constructor fields enter branch context
- **WHEN** constructor index unification succeeds
- **THEN** constructor fields are added to the branch context
- **AND** the branch expected type is specialized by the solved index refinements

### Requirement: Case Tree Artifact
Dependent pattern elaboration SHALL produce a deterministic case-tree or
equivalent core eliminator artifact for later stages.

#### Scenario: Source clauses lower to case tree
- **WHEN** accepted dependent pattern clauses are checked
- **THEN** the checker output includes a deterministic case-tree artifact
- **AND** diagnostics retain source spans from the original clauses

### Requirement: Coverage Diagnostics
Dependent pattern elaboration SHALL report missing, impossible, and redundant
branches deterministically for the accepted pattern fragment.

#### Scenario: Missing constructor is reported
- **WHEN** coverage checking finds an uncovered constructor case that is not impossible under the refined indices
- **THEN** the checker reports a missing-branch diagnostic with deterministic family and index display text

#### Scenario: Advanced equality matching is rejected
- **WHEN** a dependent pattern match would require equality-pattern reasoning outside the accepted fragment
- **THEN** the checker reports an unsupported-equality-pattern diagnostic instead of assuming an equality principle

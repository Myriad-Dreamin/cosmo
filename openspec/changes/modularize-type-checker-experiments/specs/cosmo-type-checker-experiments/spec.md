## ADDED Requirements

### Requirement: Checker Profiles
Cosmo type checking experiments SHALL be represented by named checker profiles.
Each profile SHALL declare its supported language features, rejected language
features, required inputs, produced artifact kind, and diagnostic namespace.

#### Scenario: Profile declares accepted and rejected features
- **WHEN** a checker profile is loaded by a test or package-check entry point
- **THEN** the profile metadata identifies features the checker accepts
- **AND** the profile metadata identifies features the checker deliberately rejects

#### Scenario: Existing checker has a profile
- **WHEN** the current `packages/cosmoc/src/types` expression checker is used
- **THEN** its result identifies the `cosmoc.basic-expr` checker profile

### Requirement: Profile-Aware Results
A checker result SHALL identify the checker profile that produced it and SHALL
distinguish successful typed artifacts, ordinary type errors, unsupported
features, and implementation limits.

#### Scenario: Unsupported source is rejected as a checker result
- **WHEN** a profile sees a source construct outside its declared feature set
- **THEN** the checker result contains an unsupported-feature diagnostic
- **AND** the checker does not produce typed artifacts that pretend the construct was checked

#### Scenario: Artifact kind is profile-owned
- **WHEN** two checker profiles produce different typed artifact formats
- **THEN** each result identifies its artifact kind
- **AND** the shared result schema does not require both profiles to use one type-store representation

### Requirement: cosmo0 And cosmoc Profile Alignment
cosmo0 and `packages/cosmoc` SHALL use the same conceptual checker profile names
for comparable behavior even when the implementations are written in different
languages.

#### Scenario: cosmo0 subset profile is selected
- **WHEN** a cosmo0 subset-checking test selects the subset checker
- **THEN** the observable result identifies the `cosmo0.subset` profile

#### Scenario: Profile conformance is tested by behavior
- **WHEN** two implementations claim the same conceptual checker behavior
- **THEN** conformance is judged by accepted source, rejected source, diagnostics, and deterministic artifact summaries
- **AND** conformance does not require a shared implementation

### Requirement: Experimental Feature Isolation
Checker profiles SHALL be allowed to reject features that other profiles accept.

#### Scenario: MLTT profile rejects non-MLTT features
- **WHEN** an MLTT-only profile receives source that uses traits, async, object dispatch, or C++ imports before that profile supports them
- **THEN** the profile reports unsupported-feature diagnostics
- **AND** the rejection does not imply that the same source is invalid for all checker profiles

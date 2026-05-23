## 1. Profile Model

- [x] 1.1 Define the checker profile data model in OpenSpec and test docs.
- [x] 1.2 Name the existing `packages/cosmoc/src/types/check.cos` behavior as `cosmoc.basic-expr`.
- [x] 1.3 Name the cosmo0 subset checker behavior as `cosmo0.subset`.
- [x] 1.4 Define the first unsupported-feature diagnostic code list shared by experiments.

## 2. cosmoc Implementation

- [x] 2.1 Add a small checker-profile enum or descriptor module under `packages/cosmoc/src/types`.
- [x] 2.2 Wrap the current expression checker result with a profile id and artifact tag.
- [x] 2.3 Add test helper support for selecting `cosmoc.basic-expr`.
- [x] 2.4 Add negative tests that verify unsupported feature diagnostics are checker results, not internal errors.

## 3. cosmo0 Implementation

- [x] 3.1 Add a cosmo0-side profile name for the subset checker.
- [x] 3.2 Emit profile-aware diagnostics for source rejected by the cosmo0 subset checker.
- [x] 3.3 Add fixture metadata or test harness parameters for checker profile selection.
- [x] 3.4 Confirm existing cosmo0 tests still run through the default profile.

## 4. Experimental Profile Readiness

- [x] 4.1 Add empty `mltt.core` profile metadata without making it the default.
- [x] 4.2 Add fixture expectations for sources accepted by `cosmoc.basic-expr` but rejected by `mltt.core`.
- [x] 4.3 Add fixture expectations for sources rejected by `cosmoc.basic-expr` but reserved for later MLTT support.
- [x] 4.4 Document how a future checker declares support for effects, traits, object dispatch, and dependent patterns.

## 5. Validation

- [x] 5.1 Run the focused `packages/cosmoc` type-checker tests.
- [x] 5.2 Run the cosmo0 subset checker tests.
- [x] 5.3 Add one profile-summary snapshot that proves typed artifacts remain deterministic.
- [x] 5.4 Document remaining profile-selection limitations.

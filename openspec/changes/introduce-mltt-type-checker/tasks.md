## 1. Documentation

- [x] 1.1 Add MLTT guidance covering concepts, checker shape, conversion, and implementation trade-offs.
- [x] 1.2 Cross-reference the guidance from the MLTT proposal or later book index.
- [x] 1.3 Add short examples for Pi, Sigma, equality, Nat, Vec, and conversion.
- [x] 1.4 Add a section explaining why effects, traits, and object safety are outside the first MLTT profile.
- [x] 1.5 Add a section explaining why advanced normalization strategies are deferred to separate research.

## 2. Profile Scaffolding

- [x] 2.1 Add an `mltt.core` checker profile descriptor in `packages/cosmoc`.
- [x] 2.2 Add a cosmo0-side profile name or conformance marker for `mltt.core`.
- [x] 2.3 Add unsupported-feature diagnostics for traits, effects, async, object dispatch, and C++ imports under `mltt.core`.
- [x] 2.4 Add fixture metadata selecting `mltt.core`.
- [x] 2.5 Add direct cosmo0-side profile-gate tests when a Scala reference implementation exists.

## 3. Core Data

- [x] 3.1 Add MLTT core term and type data under `packages/cosmoc/src/types/mltt` or an equivalent module.
- [x] 3.2 Add context entries for local variables, definitions, and universe levels.
- [x] 3.3 Add declaration metadata for transparent definitions and inductive families.
- [x] 3.4 Add deterministic display helpers for core terms and types.

## 4. Bidirectional Checker

- [x] 4.1 Implement `check` and `infer` entry points for variables, universes, Pi, lambda, application, and let.
- [x] 4.2 Implement diagnostics for unknown variables, non-function application, universe mismatch, and type mismatch.
- [x] 4.3 Implement simple metavariable records and conservative solving hooks.
- [x] 4.4 Add focused tests for check/infer behavior.

## 5. Conversion And Normalization

- [x] 5.1 Implement the first `mltt.whnf-conversion` normalization strategy for the initial core.
- [x] 5.2 Implement conversion for beta-reduced functions, lets, universes, Pi, and variables.
- [x] 5.3 Reject effectful or non-transparent definitions during conversion.
- [x] 5.4 Add conversion tests for `add(Z, n)`-style transparent definitions once Nat fixtures exist.
- [x] 5.5 Add diagnostics that make unsupported normalization-profile selection explicit.

## 6. Inductive Family Smoke Support

- [x] 6.1 Add declaration metadata for Nat.
- [x] 6.2 Add declaration metadata for Vec.
- [x] 6.3 Check simple Vec-indexed signatures without dependent pattern matching.
- [x] 6.4 Add diagnostics for impossible or unsupported constructor elaboration cases.

## 7. cosmo0 And cosmoc Validation

- [x] 7.1 Add `packages/cosmoc` tests for the MLTT profile.
- [x] 7.2 Add cosmo0 conformance fixtures or Scala mirror tests for the same accepted and rejected examples.
- [x] 7.3 Confirm the existing `cosmoc.basic-expr` checker remains the default for current parser-source tests.
- [x] 7.4 Document implementation gaps before enabling any package-level MLTT selection.
- [x] 7.5 Confirm cosmo0-side experiments remain profile-gated and do not expand the default cosmo0 source subset.

## 1. Documentation

- [ ] 1.1 Add MLTT guidance covering concepts, checker shape, conversion, and implementation trade-offs.
- [ ] 1.2 Cross-reference the guidance from the MLTT proposal or later book index.
- [ ] 1.3 Add short examples for Pi, Sigma, equality, Nat, Vec, and conversion.
- [ ] 1.4 Add a section explaining why effects, traits, and object safety are outside the first MLTT profile.
- [ ] 1.5 Add a section explaining why advanced normalization strategies are deferred to separate research.

## 2. Profile Scaffolding

- [ ] 2.1 Add an `mltt.core` checker profile descriptor in `packages/cosmoc`.
- [ ] 2.2 Add a cosmo0-side profile name or conformance marker for `mltt.core`.
- [ ] 2.3 Add unsupported-feature diagnostics for traits, effects, async, object dispatch, and C++ imports under `mltt.core`.
- [ ] 2.4 Add fixture metadata selecting `mltt.core`.

## 3. Core Data

- [ ] 3.1 Add MLTT core term and type data under `packages/cosmoc/src/types/mltt` or an equivalent module.
- [ ] 3.2 Add context entries for local variables, definitions, and universe levels.
- [ ] 3.3 Add declaration metadata for transparent definitions and inductive families.
- [ ] 3.4 Add deterministic display helpers for core terms and types.

## 4. Bidirectional Checker

- [ ] 4.1 Implement `check` and `infer` entry points for variables, universes, Pi, lambda, application, and let.
- [ ] 4.2 Implement diagnostics for unknown variables, non-function application, universe mismatch, and type mismatch.
- [ ] 4.3 Implement simple metavariable records and conservative solving hooks.
- [ ] 4.4 Add focused tests for check/infer behavior.

## 5. Conversion And Normalization

- [ ] 5.1 Implement the first `mltt.whnf-conversion` normalization strategy for the initial core.
- [ ] 5.2 Implement conversion for beta-reduced functions, lets, universes, Pi, and variables.
- [ ] 5.3 Reject effectful or non-transparent definitions during conversion.
- [ ] 5.4 Add conversion tests for `add(Z, n)`-style transparent definitions once Nat fixtures exist.
- [ ] 5.5 Add diagnostics that make unsupported normalization-profile selection explicit.

## 6. Inductive Family Smoke Support

- [ ] 6.1 Add declaration metadata for Nat.
- [ ] 6.2 Add declaration metadata for Vec.
- [ ] 6.3 Check simple Vec-indexed signatures without dependent pattern matching.
- [ ] 6.4 Add diagnostics for impossible or unsupported constructor elaboration cases.

## 7. cosmo0 And cosmoc Validation

- [ ] 7.1 Add `packages/cosmoc` tests for the MLTT profile.
- [ ] 7.2 Add cosmo0 conformance fixtures for the same accepted and rejected examples.
- [ ] 7.3 Confirm the existing `cosmoc.basic-expr` checker remains the default for current parser-source tests.
- [ ] 7.4 Document implementation gaps before enabling any package-level MLTT selection.

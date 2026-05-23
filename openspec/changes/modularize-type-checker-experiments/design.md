## Context

Cosmo currently has a pragmatic type-checking path under
`packages/cosmoc/src/types`. It models primitive, user, function, reference,
never, and error types, then checks a parser-source subset of expressions. That
path is intentionally narrow and useful for staged bootstrap work, but it is not
the right shape for every future type-system experiment.

The project now has several likely checker directions:

- a parser-subset checker for staged cosmo1 bootstrap;
- a cosmo0 subset checker that rejects full-language features before lowering;
- an MLTT-oriented checker for dependent type experiments;
- future trait/effect/object-safety checkers;
- future dependent pattern matching elaborators that may only work over MLTT
  core artifacts.

Those checkers should be allowed to share syntax, name-resolution output,
diagnostics, type stores, and test fixtures while still declaring different
feature support. A checker that does not support traits, effects, dependent
patterns, or object dispatch must reject those features explicitly instead of
pretending to have checked them.

## Goals / Non-Goals

**Goals:**

- Define a stable boundary between parsed/resolved source and type-checking
  experiments.
- Let a package, module, fixture, or test select a checker profile.
- Let each checker declare supported features and unsupported features.
- Keep unsupported-feature diagnostics first-class and testable.
- Keep checker outputs deterministic and comparable across experiments.
- Let cosmo0 and `packages/cosmoc` host the same conceptual checker profiles
  even when their implementations are written in different languages.
- Keep implementation slices small enough that a first profile can land in
  roughly a thousand lines of design plus implementation work.

**Non-Goals:**

- Replace the existing expression checker immediately.
- Require the MLTT checker to accept all Cosmo syntax.
- Require every checker to support traits, effects, async, object safety,
  dependent pattern matching, or C++ lowering.
- Define final surface syntax for checker selection.
- Make checker selection part of ordinary user-facing package semantics before
  the experiment model is proven.

## Decisions

### Use Explicit Checker Profiles

A checker profile is a named capability bundle:

```text
checker profile:
  id
  implementation owner
  accepted source stage
  supported features
  rejected features
  input artifact contract
  output artifact contract
  diagnostic namespace
```

Example profile names:

```text
cosmoc.basic-expr
cosmo0.subset
mltt.core
mltt.patterns
```

This keeps the language feature question separate from the implementation
question. `mltt.core` can reject traits and effects in its first stage while a
later `mltt.effects` profile can accept effect rows.

Alternative considered: use feature flags on one global checker. That makes the
checker harder to reason about and encourages hidden interactions between
experiments.

### Standardize Checker Inputs

The minimum checker input should be:

```text
TypedCheckInput:
  package/module identity
  syntax arenas
  name-resolution output
  declaration-signature output when available
  selected checker profile
  feature gates requested by the fixture or package
```

Different profiles can require additional inputs, but they must state those
requirements in the profile. For example, dependent pattern matching may require
constructor metadata and inductive-family declarations; the current basic
expression checker does not.

Alternative considered: each checker reads arbitrary compiler globals. That
would make experiments fast initially but brittle and hard to compare.

### Standardize Checker Outputs

The minimum checker output should include:

```text
TypedCheckResult:
  checker profile id
  accepted/rejected status
  diagnostics
  typed artifacts produced
  unsupported features encountered
  implementation limits encountered
```

Typed artifacts can be profile-specific:

- basic expression checker: typed expression map;
- MLTT checker: elaborated core terms and inferred core types;
- dependent pattern checker: case tree and coverage result.

The shared result should not force every checker to use the same type store.
Instead, each profile declares the artifact kind it produces.

Alternative considered: force one global `TypedExpr` model. That would block MLTT
and dependent pattern experiments because they need binders, universes,
conversion, constraints, and elaboration metadata that are not part of the
current parser-subset model.

### Unsupported Features Are Expected Results

Unsupported-feature diagnostics must be ordinary checker results:

```text
cosmo.type.unsupported-feature
cosmo.type.unsupported-dependent-pattern
cosmo.type.unsupported-effect-row
cosmo.type.unsupported-trait-constraint
```

A checker must reject unsupported source before producing misleading typed
artifacts. This is especially important for MLTT experiments: a small MLTT core
checker can be valid and useful even if it rejects traits, async, object
dispatch, and C++ imports in its first stage.

Alternative considered: treat unsupported features as internal errors. That
would make experiments look broken even when the checker is correctly scoped.

### Keep cosmo0 and cosmoc Aligned but Not Identical

cosmo0 and `packages/cosmoc` should expose the same conceptual profile names,
but they do not need identical implementations at first.

For example:

```text
cosmo0:
  cosmo0.subset implemented in Scala

packages/cosmoc:
  cosmoc.basic-expr implemented in Cosmo source
  mltt.core eventually implemented in Cosmo source
```

Profile conformance tests should define observable behavior, not require a
shared codebase.

Alternative considered: implement every checker once and call it from both
cosmo0 and cosmoc. That is premature while cosmo1 is still bootstrapping.

### Use Capability-Based Test Fixtures

Fixtures should name the checker profile and expected result:

```text
fixture:
  checker: mltt.core
  expected: rejected
  diagnostic: cosmo.type.unsupported-effect-row
```

This allows the same source file to be accepted by one checker and rejected by
another when that is intentional.

Alternative considered: one global expected result per fixture. That blocks
checker comparison and makes experimental failures ambiguous.

## Risks / Trade-offs

- Profile sprawl -> Mitigation: require each profile to document supported
  features, owner, and test fixture coverage.
- Shared result shape becomes too generic -> Mitigation: keep only status,
  diagnostics, and artifact tags shared; profile-specific typed data remains
  profile-owned.
- Users depend on experimental checker selection -> Mitigation: keep profile
  selection under development/test metadata until a profile is promoted.
- cosmo0 and cosmoc behavior diverges -> Mitigation: use conformance fixtures
  that exercise observable diagnostics and typed artifact summaries.
- Feature support becomes unclear -> Mitigation: unsupported-feature diagnostics
  must be tested for every deliberately rejected major feature.

## Migration Plan

1. Define the modular checker profile requirements.
2. Wrap the existing `packages/cosmoc/src/types/check.cos` behavior as the
   `cosmoc.basic-expr` profile in documentation and tests.
3. Add profile metadata to checker test helpers before changing package
   semantics.
4. Add cosmo0-side profile naming for the subset checker.
5. Add MLTT profile scaffolding behind tests.
6. Promote profile selection into package metadata only after at least two
   profiles are implemented and fixture-tested.

Rollback is additive: remove experimental profile metadata and keep the existing
type-checking path as the default.

## Open Questions

- Should profile selection live in `cosmo.json`, test fixture metadata, or both?
- Should profile IDs be user-facing, internal, or namespace-qualified by package?
- What minimal artifact summary is enough for cross-profile comparison?
- Which unsupported-feature diagnostic namespace should be shared across
  cosmo0 and cosmoc?
- Should checker profiles declare accepted effects and traits as independent
  feature flags, or as named bundles?

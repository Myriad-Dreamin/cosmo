## Why

Cosmo's type-checking work is moving from a single pragmatic checker toward a
set of modular experiments: parser-subset checking, MLTT checking, dependent
pattern elaboration, trait solving, effect checking, object safety, and
normalization strategies. Each underlying theory covers only part of the final
language and carries different performance and host-language assumptions, so the
project needs a long-term roadmap before those experiments become incompatible.

## What Changes

- Define a long-term type-checker theory roadmap that separates common compiler
  integration goals from optional theory-specific capabilities.
- Introduce a theory profile registry for MLTT, dependent pattern matching,
  trait solving, effect checking, object safety, normalization, totality, and
  erasure experiments.
- Define common checking goals and conformance expectations that every checker
  profile must report against, even when it rejects a feature as unsupported.
- Define normalization strategy profiles, including a first WHNF strategy and a
  process for evaluating stronger strategies through separate research and
  proposals.
- Require each theory profile to document its scope, invariants, unsupported
  features, performance envelope, host-language assumptions, and fixture coverage
  before integration into the compiler pipeline.
- Clarify that `packages/cosmo0` may host profile-gated Scala reference
  implementations for theory experiments while the default cosmo0 source subset
  remains small.
- Establish a test-suite matrix that can verify whether a checker is suitable
  for integration without requiring every checker to implement every feature.

## Capabilities

### New Capabilities

- `cosmo-type-checker-theory-roadmap`: Defines long-term theory profiles,
  common checking goals, normalization/value-model contracts, and conformance
  suite requirements for modular Cosmo type checkers.

### Modified Capabilities

None.

## Impact

- Adds roadmap-level OpenSpec requirements; it does not change compiler behavior
  immediately.
- Future MLTT, dependent pattern, trait, effect, object-safety, and normalization
  implementations will need to declare their theory profile and conformance
  level before becoming compiler integration candidates.
- Existing checkers can remain narrow, but they must become explicit about which
  common goals they support and which features they reject.
- cosmo0-side experiments are valid roadmap participants when they are
  profile-gated, directly tested, and not treated as default cosmo0 source
  behavior.

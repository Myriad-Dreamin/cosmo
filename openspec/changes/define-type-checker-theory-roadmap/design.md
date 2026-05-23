## Context

Cosmo now has separate proposals for modular checker profiles, an MLTT checker,
and dependent pattern elaboration. Those proposals correctly avoid forcing every
checker to support every feature. The next issue is long-term coordination:

- MLTT gives a compact dependent core, but it does not solve trait coherence,
  effect handling, object safety, termination, erasure, or backend lowering by
  itself.
- Dependent pattern matching can refine indexed types, but it depends on a
  particular fragment of unification, coverage checking, and equality theory.
- Normalization techniques are not complete type checkers. They carry their own
  formal assumptions, implementation models, and host-language constraints.
- Trait solving, effect checking, and object safety each have their own goal
  systems and performance risks.
- Some theories are intentionally incomplete for the full language, and some are
  too expensive to run in every compiler mode.

The roadmap should make those limits explicit. A theory profile can be useful
even if it only covers a subset, as long as the compiler knows how to test it and
how to reject unsupported programs deterministically.

The roadmap also needs a practical experimentation lane. `packages/cosmo0` is
allowed to host small Scala reference implementations for theory profiles when
that gives better diagnostics and faster feedback. This does not widen the
ordinary cosmo0 source subset; it creates profile-gated compiler infrastructure
that can be compared with the Cosmo-written `packages/cosmoc` implementation.

## Goals / Non-Goals

**Goals:**

- Define the long-term theory profile registry.
- Define common checking goals shared by all checker profiles.
- Define conformance levels for integration into cosmo0 and `packages/cosmoc`.
- Separate normalization strategy profiles from type-system profiles.
- Require stronger normalization strategies to declare their formal and
  host-language assumptions before implementation.
- Keep the MLTT first slice narrow while still leaving room for a stronger
  normalization reference checker later.
- Define a test-suite matrix that can evaluate partial checkers without
  overclaiming support.
- Define how cosmo0-side experiments participate in conformance without
  becoming default source behavior.

**Non-Goals:**

- Choose one final type theory for all of Cosmo.
- Select a stronger normalization strategy for the first MLTT implementation.
- Require dependent pattern matching before the MLTT core checker exists.
- Require trait solving, effect checking, object safety, totality checking, or
  erasure in the first MLTT checker.
- Replace existing parser-subset type checking.
- Fully specify final surface syntax for all advanced type-system features.
- Treat every cosmo0-side experiment as eligible for package or LSP integration
  before it passes the required conformance level.

## Decisions

### Use Theory Profiles

A theory profile records what a checker claims to implement:

```text
TheoryProfile:
  id
  owner checker profile
  formal model
  supported judgments
  supported source/core features
  unsupported features
  invariants required for soundness
  performance envelope
  host/value-model assumptions
  conformance suite groups
```

Initial profiles:

```text
common.harness
common.simple-types
mltt.core
mltt.whnf-conversion
mltt.inductive-families
mltt.totality
mltt.erasure
dependent-pattern.constructor-refinement
traits.coherent-solving
effects.rows
objects.safety
```

Alternative considered: one global "full type checker" profile. That hides the
fact that different theories solve different problems and makes partial
implementations look like failures.

### Allow cosmo0 Reference Implementations

Theory profiles can have more than one implementation. A profile may have a
Scala cosmo0 reference implementation, a Cosmo-written `packages/cosmoc`
implementation, or both. The conformance contract is the same: profile id,
supported and rejected goals, deterministic diagnostics, and artifact summaries.

cosmo0 reference implementations are useful when the experiment needs quick
iteration over data models, diagnostics, or coverage algorithms. They must stay
behind explicit profile selection and must not silently change the default
`cosmo0.subset` behavior.

Alternative considered: permit only `packages/cosmoc` experiments. That keeps
the bootstrap compiler smaller on paper, but it makes it harder to validate
experimental diagnostics and profile contracts while the Cosmo-written compiler
is still incomplete.

### Define Common Goals Before Theory-Specific Goals

Every checker should implement or explicitly reject a small set of common goals:

```text
Goal.CheckModule
Goal.ResolveDeclarations
Goal.WellFormedType
Goal.InferExpr
Goal.CheckExpr
Goal.Convert
Goal.UnsupportedFeature
Goal.ArtifactSummary
```

A checker can return unsupported for goals outside its scope, but the result must
be deterministic and diagnostic-bearing.

Theory-specific profiles can add:

```text
Goal.CheckCore
Goal.InferCore
Goal.Normalize
Goal.CheckTelescope
Goal.CheckInductiveFamily
Goal.ElaboratePattern
Goal.CheckCoverage
Goal.SolveTraitObligation
Goal.CheckEffectSubsumption
Goal.CheckObjectSafety
```

Alternative considered: test only whole files. Whole-file tests are necessary
for integration but too coarse to evaluate small theory slices.

### Separate Normalization Strategies

Normalization is a service used by conversion, not the whole checker. The first
MLTT implementation should expose a stable conversion API and begin with a small
WHNF-style strategy. Stronger strategies should be proposed only after a separate
research pass clarifies their formal model, host-language requirements, and
performance behavior.

The roadmap distinguishes:

```text
mltt.whnf-conversion:
  normalizes enough for first-stage conversion
  lower implementation cost
  suitable for small fixtures and staged implementation
```

Alternative considered: require a stronger normalization strategy immediately.
That pushes unresolved theory and host-language representation choices into the
first checker slice.

### Require Host And Evaluation Model Contracts

Any theory profile that evaluates terms must declare the implementation model it
depends on.

For evaluator-style normalization, a future profile may need to define:

```text
evaluation model:
  represented evaluation values, if any
  environment representation, if any
  opaque and transparent definition handling
  reification back to core syntax, if any
```

The host language must support the chosen representation deterministically.
If the checker is written in the cosmo0 subset, the implementation model must be
expressible using available core0 containers, arenas, IDs, references, and the
subset's supported control-flow and data forms.

Alternative considered: rely on host-language implementation details without a
contract. That makes it hard to port the checker between Scala cosmo0 and
Cosmo-written cosmoc.

### Treat Performance As Part Of Profile Eligibility

Each theory profile must state its expected complexity and bailout behavior:

```text
performance envelope:
  expected input size
  maximum normalization fuel or recursion depth
  cache keys
  cycle handling
  deterministic timeout/overflow diagnostics
  whether the profile is suitable for editor feedback
  whether the profile is suitable for package compilation
```

This is not premature optimization. Dependent conversion, trait solving,
coverage checking, and evaluator-style normalization can all become expensive. A
profile that is correct but too slow may still be useful as a reference checker
rather than the default compiler checker.

Alternative considered: optimize after feature completion. That risks building a
checker whose behavior cannot be used in the compiler pipeline.

### Use Conformance Levels

Profiles should graduate through levels:

```text
L0 documented:
  profile, scope, and unsupported features are documented

L1 goal-tested:
  focused goal tests pass

L2 fixture-tested:
  source/core fixtures pass with deterministic diagnostics

L3 integration-tested:
  module/package tests pass and artifact summaries are stable

L4 compiler-candidate:
  performance envelope is measured and failure modes are deterministic
```

Alternative considered: binary "implemented/not implemented" status. That is not
useful for long-running checker experiments.

### Keep Theory Boundaries Visible In Diagnostics

Diagnostics should say which profile rejected a program and why:

```text
profile: mltt.core
code: cosmo.type.unsupported-effect-in-conversion
reason: effectful term cannot be reduced by this conversion profile
```

This helps users and implementers distinguish invalid programs from unsupported
checker experiments.

Alternative considered: reuse generic type mismatch diagnostics for all
rejections. That makes conformance tests vague and theory debugging harder.

## Risks / Trade-offs

- Too many profiles -> Mitigation: require owner, scope, tests, and conformance
  level before adding a profile.
- Roadmap feels abstract -> Mitigation: every profile must map to concrete goal
  tests and fixture groups.
- Stronger normalization research is delayed too long -> Mitigation: leave an
  explicit roadmap slot and require future proposals to bring references and
  conformance tests.
- WHNF first slice becomes the de facto theory -> Mitigation: keep the conversion
  API stable and explicitly list WHNF as a first implementation strategy, not the
  final normalization story.
- Evaluation-model contract blocks implementation -> Mitigation: require it only
  for profiles that actually evaluate terms beyond the first-stage conversion
  strategy; simple checkers can remain syntax-directed.
- Partial support confuses integration -> Mitigation: conformance levels and
  unsupported-feature diagnostics are required before integration.
- cosmo0 becomes a dumping ground for unfinished theory work -> Mitigation:
  require explicit profile ownership, small focused slices, direct tests, and
  conformance reporting before any experiment is considered for integration.

## Migration Plan

1. Add this roadmap and spec requirements.
2. Update the MLTT proposal so the first implementation targets
   `mltt.core + mltt.whnf-conversion`, while stronger normalization strategies
   are deferred to later research.
3. Update MLTT guidance to describe normalization as a replaceable service and
   avoid committing to an advanced strategy before research.
4. Add a future `define-common-type-checker-goals` or equivalent implementation
   change for the goal DSL and fixture manifest.
5. Add conformance-level metadata to checker profiles.
6. Add fixture groups for common goals, MLTT core, WHNF conversion, future
   normalization strategies, dependent patterns, effects, traits, and object
   safety.
7. Add cosmo0-side reference implementations or conformance harnesses for
   experiments that need faster diagnostic and behavior iteration.
8. Promote profiles into package-level compiler integration only after they reach
   the required conformance level.

Rollback is additive: keep the roadmap as planning documentation and do not
enable any profile as a compiler default until it passes conformance gates.

## Open Questions

- Which stronger normalization strategies should be evaluated first, and what
  references are required before they become proposals?
- Should the long-term conversion API support eta-conversion, and if so should it
  be a separate proposal?
- Should conformance levels be stored in OpenSpec, fixture manifests, package
  metadata, or generated test reports?
- What is the default fuel/overflow policy for normalization and trait solving?
- Which profile level is required before a checker can run in the LSP?

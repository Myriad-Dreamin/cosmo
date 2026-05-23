## Context

The current cosmo1 type model covers bootstrap-oriented types: primitive types,
user class types, function types, references, never, and error. That model is
appropriate for staged compiler work, but it does not give a principled core for
dependent pattern matching, type-level computation, or future proof-oriented
experiments.

Martin-Lof type theory is a useful first dependent core because it can be kept
small:

- universes classify types;
- Pi types model dependent functions;
- Sigma types model dependent pairs;
- inductive families model indexed data such as `Vec[A, n]`;
- equality types model propositional equality;
- definitional equality is checked by normalization/conversion;
- dependent pattern matching can later elaborate into eliminators or case trees.

The goal is not to turn Cosmo into Agda, Idris, or Coq in one step. The goal is
to build a small, modular checker that can be compared against other checkers and
used to explore theory without making full Cosmo depend on every experiment.

## Goals / Non-Goals

**Goals:**

- Add an `mltt.core` checker profile.
- Define a compact core syntax suitable for implementation in both cosmo0-side
  infrastructure and `packages/cosmoc`.
- Use bidirectional type checking to keep inference local and diagnostics clear.
- Use a conservative conversion API backed first by a declared WHNF-style
  normalization strategy over pure, transparent terms.
- Keep effectful computations out of definitional equality.
- Keep the first implementation small enough to be a useful thousand-line-scale
  experiment with clear comments.
- Produce guidance documentation that explains the theory and implementation
  choices.

**Non-Goals:**

- Support all Cosmo features in the first MLTT profile.
- Support traits, effects, async, object safety, C++ imports, macros, reflection,
  or staging in the first profile.
- Support arbitrary higher-order unification.
- Support general recursion in type-level computation.
- Select or evaluate advanced normalization strategies in the first MLTT profile.
- Implement dependent pattern matching in this change.
- Commit to HoTT, cubical type theory, quotient types, sized types, or QTT.
- Replace the existing parser-subset checker.

## Decisions

### Use A Small MLTT Core

The first core should contain:

```text
Term:
  Var
  Universe(level)
  Pi(name, domain, body)
  Sigma(name, first, second)
  Lambda(name, annotation, body)
  Apply(function, argument)
  Pair(first, second)
  Fst(pair)
  Snd(pair)
  Let(name, value, body)
  Inductive(name, params, indices)
  Constructor(name, args)
  Eq(type, left, right)
  Refl(value)
  Neutral(head, spine)
  Error
```

This is enough to test dependent functions, simple dependent pairs, equality,
and indexed data declarations without importing the whole source language.

Alternative considered: directly extend the existing `CosmoType` enum. That
would mix parser-subset types with a binder-heavy dependent core and make both
harder to reason about.

### Use Bidirectional Checking

The checker should expose two mutually recursive modes:

```text
check(ctx, term, expected_type) -> CheckedTerm
infer(ctx, term) -> (CheckedTerm, inferred_type)
```

Examples:

- lambdas check against expected Pi types;
- variables infer from the context;
- applications infer the function type, then check the argument;
- annotated terms can bridge from check mode to infer mode;
- constructors usually check against an expected inductive-family type.

Alternative considered: full inference. That is not a good fit for dependent
types and would push too much work into unification.

### Keep Conversion Separate

Definitional equality should be a separate operation:

```text
convert(ctx, left_type, right_type, strategy) -> ConversionResult
```

The first implementation should use a declared first-stage strategy such as
`mltt.whnf-conversion`: weak-head normalization plus structural comparison over
pure, terminating, transparent terms. The conversion API should still be shaped
so later normalization strategies can be evaluated without changing checker
callers. This proposal deliberately avoids naming or committing to those later
strategies.

Alternative considered: compare raw syntax. That fails for ordinary dependent
programs where `add(Z, n)` must be definitionally equal to `n`.

Alternative considered: require a stronger normalization strategy in the first
MLTT checker. That would make the first slice depend on theory and host-language
representation choices that need separate research.

### Keep Effects Out Of Definitional Equality

The existing effect design treats `with E` as computation behavior. The MLTT
core should respect that by requiring type-level computation to be pure. Terms
with `async`, `yield`, `throw`, IO, mutation, or other unhandled effects cannot
be reduced by conversion.

This does not mean effects cannot exist in full Cosmo. It means the first MLTT
conversion checker does not run effectful computations to decide type equality.

Alternative considered: allow arbitrary compile-time execution during
conversion. That makes type checking depend on runtime effects and threatens
termination, determinism, and diagnostics.

### Use Metavariables Conservatively

The first MLTT checker should support metavariables only where they are needed
for local elaboration:

```text
MetaVar:
  id
  context snapshot
  expected type
  solution optional
```

Solving should support first-order and simple pattern-like cases. Arbitrary
higher-order unification is out of scope.

Alternative considered: no metavariables. That makes even small implicit
arguments and constructor elaboration awkward. Full higher-order unification is
too large for the first slice.

### Treat Inductive Families As Declarations, Not Magic Builtins

The first implementation should represent inductive-family signatures as
declaration metadata:

```text
InductiveDecl:
  name
  parameters
  indices
  constructors

ConstructorDecl:
  name
  telescope
  result family and indices
```

This lets `Vec[A, n]`, `Nat`, and `Fin[n]` be test declarations rather than
hard-coded checker branches.

Alternative considered: hard-code `Nat`, `Vec`, and `Fin`. That may be useful
for smoke tests, but the checker architecture should not depend on it.

### Keep Implementation Slices Small

The first implementation should aim for roughly a thousand lines per slice:

- core data and pretty printing;
- context and diagnostics;
- infer/check;
- conversion and normalization;
- a small declaration environment;
- tests and fixtures.

This is a budget guideline, not a hard size limit. The important property is
that each slice can be reviewed independently and heavily commented.

## Risks / Trade-offs

- MLTT core diverges from surface Cosmo -> Mitigation: keep it behind the
  `mltt.core` profile and require explicit elaboration.
- Conversion becomes too powerful -> Mitigation: only pure, total, transparent
  terms reduce.
- Advanced normalization requirements leak into the first checker -> Mitigation:
  first implement a WHNF-style strategy and require separate proposals for
  stronger strategies.
- Diagnostics become abstract -> Mitigation: keep source spans and elaboration
  notes attached to core terms.
- Metavariables become an implicit theorem prover -> Mitigation: support only
  conservative local solving and report postponed constraints.
- Universe design is under-specified -> Mitigation: start with predicative
  universe levels and no cumulativity unless explicitly added.
- cosmo0 cannot host the whole checker early -> Mitigation: define behavioral
  conformance first, then implement the Cosmo-written version as staged source.

## Migration Plan

1. Add the guidance document and OpenSpec requirements.
2. Add an `mltt.core` profile that rejects all source until the core input
   format is available.
3. Implement core data and display in `packages/cosmoc`.
4. Implement context, diagnostics, and bidirectional checking for variables,
   universes, Pi, lambda, application, and let.
5. Add conversion with the first `mltt.whnf-conversion` strategy for
   beta-reduction and transparent lets.
6. Add Sigma and equality.
7. Add inductive-family declaration metadata and small Nat/Vec fixtures.
8. Add a cosmo0-side conformance harness or mirror implementation when the
   Cosmo-written profile has stable behavior.
9. Evaluate stronger normalization strategies only after a separate research
   pass and proposal define the formal and implementation requirements.

Rollback is additive: remove or disable the `mltt.core` profile and leave the
existing `cosmoc.basic-expr` checker as the default.

## Open Questions

- Should the first universe policy include cumulativity or strict universe
  equality only?
- What exact WHNF cache key and fuel policy should the first conversion strategy
  use?
- How much implicit argument insertion should the first checker support?
- Should totality checking be part of `mltt.core`, or a later
  `mltt.totality` profile?
- Should erased proof/index arguments be represented in the first core or added
  after dependent pattern matching exists?
- Which normalization strategies should be studied after the first WHNF-style
  implementation, and what evidence is required before adopting one?

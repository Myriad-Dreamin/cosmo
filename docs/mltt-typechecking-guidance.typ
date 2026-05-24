= MLTT Type Checking Guidance

== Status

This document is guidance for the experimental MLTT checker. It is not yet a
normative language specification. Its purpose is to give implementers and
reviewers a shared vocabulary before Cosmo grows dependent pattern matching,
trait-aware checking, effect-aware checking, and object-safety checks.

The first implementation slice lives under
`packages/cosmoc/src/types/mltt/`, with a Scala-side mirror under
`packages/cosmo0/src/main/scala/cosmo0/MlttTypeChecker.scala`. It provides
explicit MLTT core data, profile-aware diagnostics, bidirectional check/infer
entry points, conservative metavariable records, Nat/Vec metadata fixtures, and
the first declared conversion strategy `mltt.whnf-conversion`. Selecting
`mltt.core` for ordinary Cosmo source or package checking remains isolated until
a surface elaborator can produce MLTT core artifacts.

The key design rule is:

```text
MLTT is one checker profile, not the whole language.
```

Cosmo can host multiple checker profiles. A small MLTT checker is useful even
when it rejects traits, effects, async, object dispatch, C++ imports, macros, and
dependent pattern matching in its first version.

== Why MLTT

Martin-Lof type theory gives a compact dependent core:

- `Type` universes classify types.
- `Pi` types classify dependent functions.
- `Sigma` types classify dependent pairs.
- Inductive families classify indexed data such as `Vec[A, n]`.
- Equality types classify proofs that two terms are propositionally equal.
- Conversion checks definitional equality by reducing pure transparent terms.

This is a good experimental core for Cosmo because it can support future
dependent pattern matching without requiring every checker to become a proof
assistant.

== Surface And Core

Keep two layers separate:

```text
surface Cosmo:
  friendly syntax, traits, effects, async, objects, imports, pattern clauses

MLTT core:
  explicit binders, explicit terms, explicit conversion, explicit diagnostics
```

The checker should trust the core, not the surface syntax. Surface constructs are
elaborated into core artifacts before or during checking.

== Core Judgments

The initial checker should be bidirectional:

```text
Γ |- t <= A        check term t against expected type A
Γ |- t => A        infer type A for term t
Γ |- A == B        definitional equality / conversion
Γ |- E1 <= E2      effect-row inclusion, when effect profiles are added
Γ |- O solved      trait or capability obligation, when trait profiles are added
```

The first MLTT profile only needs the first three judgments. Effect and trait
judgments are listed because later profiles should layer them around the core
instead of hiding them inside conversion.

== Core Terms

A useful first core has these shapes:

```text
Var(name or index)
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

Implementers can choose de Bruijn indices, de Bruijn levels, or stable local IDs.
For Cosmo compiler code, stable IDs plus explicit context entries are easier to
debug. For conversion and substitution, de Bruijn levels often make
alpha-equivalence and reification back to core syntax cleaner.

== Short Examples

Pi type:

```text
(A: Type0) -> (x: A) -> A
```

Sigma type:

```text
Sigma(x: A). B(x)
```

Equality type:

```text
Eq(A, x, x)
Refl(x)
```

Nat metadata:

```text
Nat : Type0
Z   : Nat
S   : Nat -> Nat
```

Vec metadata:

```text
Vec(A: Type0, n: Nat) : Type0
Nil  : Vec(A, Z)
Cons : (k: Nat) -> A -> Vec(A, k) -> Vec(A, S(k))
```

Conversion:

```text
(fun x: A => x)(y) == y
let z = y; z == y
```

== Bidirectional Checking

Use check mode when the expected type is known:

```text
check(lambda, Pi)
check(pair, Sigma)
check(constructor, expected inductive family)
```

Use infer mode when the term naturally exposes its type:

```text
infer(variable)
infer(application)
infer(projection)
infer(annotated term)
```

This avoids pretending that a dependent language has complete inference. It also
gives better diagnostics because the checker knows whether it was trying to
check against a specific type or synthesize one.

== Conversion

Conversion answers:

```text
Are these two core terms definitionally equal?
```

The first implementation should only reduce:

- beta-redexes from applying lambdas;
- transparent pure lets;
- transparent total definitions admitted by the MLTT profile;
- eliminator or case reductions only after those features are introduced.

It must not run:

- async computations;
- generator/yield computations;
- throwing computations;
- IO;
- mutation;
- arbitrary compile-time reflection;
- general recursive definitions.

This boundary keeps type checking deterministic and prevents effectful programs
from deciding type equality.

== Normalization Strategy

Normalization is a service used by conversion. It is not the whole checker.

The first MLTT implementation should expose a stable conversion API and use a
small first-stage strategy:

```text
WHNF + structural comparison:
  simpler to implement incrementally
  enough for many early tests
  can need repeated normalization in nested comparisons
```

This first profile can be named:

```text
mltt.whnf-conversion
```

Other normalization strategies need separate research before they become design
commitments. This guidance intentionally does not evaluate or recommend a
specific advanced strategy. Any later proposal should bring its own references,
formal model, host-language assumptions, performance expectations, and
conformance tests.

== Universes

Use predicative universe levels first:

```text
Type0 : Type1
Type1 : Type2
...
```

Do not add cumulativity unless it is a deliberate proposal:

```text
Type0 <= Type1
```

Cumulativity improves ergonomics but adds constraints and diagnostic complexity.
Strict universe equality is easier for the first implementation.

== Inductive Families

Represent inductive families as declarations:

```text
Inductive Vec(A: Type0, n: Nat): Type0
  Nil  : Vec(A, Z)
  Cons : (k: Nat) -> A -> Vec(A, k) -> Vec(A, S(k))
```

Constructor checking compares the expected family with the constructor result.
For dependent pattern matching, matching a constructor will later refine indices
by unifying the scrutinee family with the constructor result.

== Equality

The first equality type can be intensional:

```text
Eq(A, x, y)
```

`Refl(x)` checks against `Eq(A, x, x)` when both sides are definitionally equal.

Do not silently assume advanced equality principles such as K, UIP, univalence,
or quotient equality. If Cosmo later accepts one of those principles, it should
be an explicit proposal because it changes the theory and dependent pattern
matching rules.

== Metavariables

Metavariables are useful for elaboration:

```text
?m : A in context Γ
```

The first checker should solve only conservative cases:

- exact known term;
- first-order constructor-shaped constraints;
- simple pattern variables.

Arbitrary higher-order unification should remain unsupported. When a
metavariable cannot be solved, report a deterministic postponed or unsolved
constraint instead of guessing.

== Effects And Traits

Effects and traits are not part of the first MLTT conversion relation.

Later profiles can add:

```text
Γ |- effects(body) <= declared effects
Γ |- T implements Trait
Γ |- object-safe(Trait, ABI)
```

Those checks should produce obligations around the core checker. They should not
turn definitional equality into trait search or effect execution.

== Dependent Pattern Matching Preview

Dependent pattern matching should elaborate into core case trees or eliminators.
When matching:

```text
xs : Vec(A, S(n))
```

against `Nil`, the checker tries to unify:

```text
Vec(A, S(n)) == Vec(A, Z)
```

which fails because `S(n)` and `Z` do not unify. The branch is impossible.

When matching against `Cons`, the branch learns:

```text
xs = Cons(k, head, tail)
tail : Vec(A, k)
S(k) == S(n)
```

and the expected result type is specialized under those refinements.

This is why dependent pattern matching should be implemented as an elaborator
over MLTT core metadata, not as ad hoc expression typing.

== Implementation Shape

Suggested modules:

```text
types/mltt/core.cos          term, type, binder, declaration data
types/mltt/context.cos       local context and lookup
types/mltt/diagnostic.cos    profile-aware diagnostics
types/mltt/normalize.cos     first-stage normalization strategy
types/mltt/convert.cos       definitional equality
types/mltt/check.cos         bidirectional check/infer
types/mltt/fixture.cos       small Nat/Vec examples
```

Keep functions small and explicit. Prefer helper functions for repeated
diagnostic and context-extension logic. The checker should read top to bottom:
infer a shape, validate it, emit obligations, return a typed artifact.

== First Test Set

Start with:

- `Type0 : Type1`;
- identity function checks against `(A: Type0) -> A -> A`;
- application of identity infers the argument type;
- pair checks against a Sigma type;
- `Refl(x)` checks against `Eq(A, x, x)`;
- mismatch between `Type0` and a non-universe reports a diagnostic;
- effectful source is rejected by `mltt.core`;
- trait source is rejected by `mltt.core`.

After that, add Nat and Vec fixtures before dependent pattern matching.

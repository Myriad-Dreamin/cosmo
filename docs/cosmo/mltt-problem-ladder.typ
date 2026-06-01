#import "mod.typ": *

#show: book-page.with(title: "MLTT Problem Ladder")

= MLTT Problem Ladder

== Status

This page is a source-tagged problem ladder for the experimental `mltt.core`
checker. It is a collection of implementation exercises and fixture candidates,
not a normative Cosmo language specification. Each entry uses one of these
statuses:

- `supported-now`: the current `mltt.core` checker should have an executable
  fixture or Scala mirror test.
- `source-backed-future`: the problem is backed by external dependent-type
  implementation material, but the current checker deliberately does not support
  it yet.
- `paper-exercise`: the problem is useful as a derivation or design exercise,
  but it is not yet a compiler fixture.

== Source Index

- `elaboration-zoo`: #link("https://github.com/AndrasKovacs/elaboration-zoo")[Andras Kovacs, elaboration-zoo]. This is a sequence of small dependent type checking and elaboration implementations. It is the main source for evaluation representations, bidirectional checking, holes, implicit arguments, pruning, and first-class polymorphism.
- `nbe-tutorial`: #link("https://davidchristiansen.dk/tutorials/nbe/")[David Christiansen, Checking Dependent Types with Normalization by Evaluation]. This is the main source for conversion, normalization, equality checking, holes, and project-style extensions such as vectors.
- `cmu-dependency-notes`: #link("https://www.cs.cmu.edu/~rwh/courses/atpl/pdfs/dependency.pdf")[CMU ATPL dependency notes]. These notes are the source for Pi, Sigma, identity type rules, and eta-style exercises.
- `agda-implicit-args`: #link("https://wiki.portal.chalmers.se/agda/ReferenceManual/FindingTheValuesOfImplicitArguments")[Agda reference manual, Finding the Values of Implicit Arguments]. This is a practical source for implicit argument metavariables and restricted pattern unification.
- `lean-elaboration`: #link("https://leanprover-community.github.io/lean4-metaprogramming-book/main/02_overview.html")[Lean 4 Metaprogramming Book, elaboration overview]. This is a practical source for surface holes becoming metavariables during elaboration.
- `practical-unification`: #link("https://research.chalmers.se/en/publication/519011")[Practical Unification for Dependent Type Checking]. This is the source for the boundary around higher-order unification and practical restrictions.
- `pi-forall`: #link("https://arxiv.org/abs/2207.02129")[pi-forall paper] and #link("https://github.com/sweirich/pi-forall")[pi-forall repository]. These are sources for small dependently typed language implementation exercises beyond the current checker.
- `radboud-exercises`: #link("https://www.cs.ru.nl/~herman/TT/opg6.pdf")[Radboud Type Theory Exercises 6]. This is a source for proof-term and derivation exercises.

== Supported Now

problem: `mltt-core-display`
status: `supported-now`
sources: `elaboration-zoo`, `nbe-tutorial`

Exercise: build the same small core term set using explicit constructors and
prove that display is deterministic for universes, Pi, Sigma, equality, Nat,
and Vec. This is the smallest check that the implementation has a stable core
syntax before type checking is involved.

problem: `mltt-universe-variable`
status: `supported-now`
sources: `elaboration-zoo`, `cmu-dependency-notes`

Exercise: infer `Type0 : Type1` and infer a variable from the local context.
The context lookup case must report the `mltt.core` profile on unknown
variables.

problem: `mltt-pi-lambda-application`
status: `supported-now`
sources: `elaboration-zoo`, `cmu-dependency-notes`

Exercise: check an identity lambda against a Pi type and infer an application
through the Pi codomain substitution. Lambdas should use check mode when an
expected Pi is available; applications should infer the function type and then
check the argument.

problem: `mltt-sigma-refl`
status: `supported-now`
sources: `cmu-dependency-notes`, `nbe-tutorial`

Exercise: check a pair against a Sigma type and check `Refl(x)` against
`Eq(A, x, x)`. The equality case should use conversion, not raw syntax, for the
two endpoints.

problem: `mltt-projection-whnf`
status: `supported-now`
sources: `nbe-tutorial`, `cmu-dependency-notes`

Exercise: reduce `fst((x, y))` to `x` and `snd((x, y))` to `y` during WHNF
conversion. This keeps dependent pair projections in the conversion relation
without requiring a full normalizer.

problem: `mltt-conversion-beta-let-def`
status: `supported-now`
sources: `nbe-tutorial`, `elaboration-zoo`

Exercise: accept beta conversion, transparent let conversion, and transparent
pure definition unfolding, such as an `add(Z, n)`-style fixture reducing to
`n`.

problem: `mltt-conversion-effect-boundary`
status: `supported-now`
sources: `nbe-tutorial`

Exercise: reject conversion through opaque or effectful definitions, and reject
unknown normalization strategy names. This preserves the design rule that
definitional equality is pure, deterministic type-level computation.

problem: `mltt-nat-vec-constructors`
status: `supported-now`
sources: `nbe-tutorial`, `agda-implicit-args`

Exercise: record Nat and Vec declaration metadata, check `Z` and `S(Z)` against
Nat, check `Nil` against `Vec(A, Z)`, and check `Cons(k, head, tail)` against
`Vec(A, S(k))`. This is constructor-signature checking only; impossible branch
analysis belongs to dependent pattern elaboration.

problem: `mltt-meta-exact-higher-order-reject`
status: `supported-now`
sources: `elaboration-zoo`, `agda-implicit-args`, `practical-unification`

Exercise: create a local metavariable with a context snapshot, solve it exactly
when a known term is supplied, and reject arbitrary higher-order unification
with a deterministic diagnostic.

== Source-Backed Future Problems

problem: `mltt-surface-holes`
status: `source-backed-future`
sources: `elaboration-zoo`, `lean-elaboration`, `nbe-tutorial`

Exercise: elaborate a surface `_` hole into a metavariable that records the
current context and expected type. The current checker has core metavariable
records, but no surface elaborator that creates them from syntax.

problem: `mltt-implicit-arguments`
status: `source-backed-future`
sources: `elaboration-zoo`, `agda-implicit-args`, `lean-elaboration`

Exercise: insert implicit argument metavariables while elaborating a function
call, then solve them from explicit arguments or the expected result type. The
current core has no explicit/implicit Pi distinction.

problem: `mltt-pruning-pattern-unification`
status: `source-backed-future`
sources: `elaboration-zoo`, `agda-implicit-args`, `practical-unification`

Exercise: solve only restricted pattern constraints, prune unused metavariable
dependencies, and postpone or reject non-pattern constraints. The current
checker only exposes conservative exact solving and a higher-order rejection
diagnostic.

problem: `mltt-first-class-polymorphism`
status: `source-backed-future`
sources: `elaboration-zoo`, `pi-forall`

Exercise: pass polymorphic values through the core while preserving explicit and
implicit function boundaries. This is outside the current `mltt.core` term
language.

problem: `mltt-dependent-pattern-impossible-branch`
status: `source-backed-future`
sources: `nbe-tutorial`, `agda-implicit-args`

Exercise: elaborate a match on `xs : Vec(A, S(n))` and mark the `Nil` branch as
impossible by unifying `Vec(A, S(n))` with the constructor result `Vec(A, Z)`.
This belongs to the dependent-pattern checker profile, not the first core
checker.

problem: `mltt-identity-eliminator-j`
status: `source-backed-future`
sources: `cmu-dependency-notes`

Exercise: add the identity eliminator J and its beta rule while keeping the
theory intensional unless a later proposal explicitly accepts additional
equality principles.

problem: `mltt-eta-cumulativity`
status: `source-backed-future`
sources: `cmu-dependency-notes`, `nbe-tutorial`

Exercise: evaluate whether Pi/Sigma eta rules or universe cumulativity should be
part of a later profile. The current checker keeps strict universe levels and
does not add eta conversion.

== Paper Exercises

problem: `mltt-logic-proof-term-derivations`
status: `paper-exercise`
sources: `radboud-exercises`, `cmu-dependency-notes`

Exercise: derive a proof term and context typing derivation for a small
predicate-logic encoding. This is useful reviewer training for dependent
judgments, but it should not become a compiler fixture until Cosmo has a surface
syntax for those proof terms.

## Context

Ordinary pattern matching chooses a branch based on runtime constructor shape.
Dependent pattern matching also refines the type context. For an indexed family
such as:

```text
Vec(A, n)
```

matching a value of type `Vec(A, S(n))` against `Nil` is impossible because the
constructor result is `Vec(A, Z)`. Matching against `Cons` refines the branch
context with the constructor fields and the equality between expected and actual
indices.

Cosmo should support this without making every checker implement the feature. A
basic expression checker can reject dependent patterns; `cosmo0.subset` can
declare support for the bootstrap surface; an MLTT checker can first support the
core types; and a later profile can add MLTT-specific dependent pattern
elaboration.

## Goals / Non-Goals

**Goals:**

- Define dependent pattern matching as an optional checker/elaborator capability.
- Elaborate source pattern clauses into deterministic case trees or core
  eliminator forms.
- Use constructor unification to refine indexed types in branch contexts.
- Detect impossible branches and missing coverage deterministically.
- Preserve source spans for diagnostics.
- Keep the first implementation small enough to be reviewed as an independent
  thousand-line-scale slice.

**Non-Goals:**

- Implement dependent pattern matching in the first MLTT checker change.
- Support arbitrary higher-order unification.
- Support pattern synonyms, views, inaccessible/dot patterns, or with-abstraction
  in the first slice.
- Decide whether Cosmo assumes K/UIP.
- Support object dispatch over dependent pattern methods.

## Decisions

### Make Dependent Patterns A Profile Feature

A checker profile must declare:

```text
supports dependent-pattern-elaboration: yes/no
```

If the profile does not support the feature, it must reject source with a
diagnostic such as:

```text
cosmo.type.unsupported-dependent-pattern
```

Alternative considered: make dependent patterns part of all match typing. That
would force every checker to support indexed unification and coverage, including
the intentionally smaller `cosmoc.basic-expr` and `mltt.core` profiles.

### Elaborate Through Constructor Metadata

The elaborator should use inductive-family metadata:

```text
ConstructorDecl:
  name
  telescope
  result family
  result indices
```

For each constructor pattern, it unifies the scrutinee type with the constructor
result type, extends the branch context with constructor fields, and specializes
the expected branch result type under the solved refinements.

Alternative considered: hard-code common families such as `Nat`, `Vec`, and
`Fin`. That is useful for tests but not a real language design.

### Generate Case Trees

Pattern clauses should elaborate to deterministic case trees:

```text
case scrutinee of
  Constructor(args) -> branch
  ...
```

Case trees should be independent of source clause ordering where possible, but
diagnostics must still point at source clauses. The case tree is the artifact
later lowering stages consume.

Alternative considered: type-check clauses directly and lower from surface
syntax. That makes coverage, redundancy, and future optimization harder.

### Keep Unification Conservative

The first implementation should support:

- constructor-head comparison;
- first-order index variables;
- definitional equality after allowed normalization;
- occurs check;
- failure-as-impossible for constructor index mismatch;
- postponed constraints for unsupported cases.

It should not support arbitrary higher-order unification.

Alternative considered: import a full proof assistant unifier. That is too large
for the first Cosmo implementation slice.

### Treat Impossible Branches Explicitly

An impossible branch is different from a runtime-unreachable branch. It is a
branch whose pattern cannot match because constructor indices cannot unify with
the scrutinee type.

The checker should allow an explicit absurd marker later, but the first slice can
start by detecting impossible generated branches during coverage.

Alternative considered: require users to write all constructors and then ignore
impossible ones. That harms diagnostics and obscures the dependent typing model.

### Do Not Decide K/UIP In The First Slice

Dependent pattern matching over equality can imply additional equality
principles. The first proposal should avoid committing to K/UIP by limiting the
initial feature to ordinary inductive-family constructor refinement and by
rejecting advanced equality matching cases.

Alternative considered: assume K/UIP globally. That may be acceptable for a
systems language, but it should be a deliberate theory proposal.

## Risks / Trade-offs

- Dependent pattern elaboration becomes too broad -> Mitigation: gate it by
  profile and start with constructor patterns only.
- Coverage diagnostics become confusing -> Mitigation: keep source spans and
  branch refinement summaries in diagnostics.
- Unification gets stuck too often -> Mitigation: report postponed constraints
  deterministically and keep examples within the supported fragment.
- Theory commitment is hidden -> Mitigation: reject equality-pattern cases that
  would require K/UIP until a separate proposal decides that principle.
- Lowering depends on surface syntax -> Mitigation: lower from deterministic
  case trees, not raw pattern clauses.

## Migration Plan

1. Add capability requirements and profile gating.
2. Add source AST pattern metadata if current AST cannot represent constructor
   patterns with spans and arguments cleanly.
3. Add constructor metadata to the MLTT declaration environment.
4. Implement a small unification/refinement module for family indices.
5. Implement clause elaboration into case trees.
6. Implement coverage and impossible-branch diagnostics.
7. Add Nat and Vec fixtures.
8. Add cosmo0 and cosmoc conformance tests that show `cosmo0.subset` and the
   dependent-pattern profile accept the feature metadata while unsupported
   profiles reject dependent patterns.

Rollback is additive: disable the dependent-pattern profile and keep ordinary
match typing unchanged.

## Open Questions

- What source spelling should mark absurd branches?
- Should inaccessible/dot patterns be part of the first accepted syntax?
- Should coverage use a matrix algorithm from the start or a simpler
  constructor-split worklist?
- When should `with`-style dependent refinement be introduced?
- Should Cosmo assume K/UIP, reject K-sensitive matches, or expose a theory
  setting per profile?

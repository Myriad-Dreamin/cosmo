## 1. Profile And Spec

- [x] 1.1 Add profile metadata for dependent-pattern support.
- [x] 1.2 Add unsupported-dependent-pattern diagnostics for profiles that do not support the feature.
- [x] 1.3 Define the first accepted pattern fragment: constructor, variable, wildcard, and impossible cases.
- [x] 1.4 Define which equality-pattern cases remain rejected.

## 2. Core Metadata

- [x] 2.1 Extend MLTT declaration metadata with constructor telescopes and result indices.
- [x] 2.2 Add deterministic display for indexed family types and constructor result types.
- [x] 2.3 Add fixture declarations for Nat.
- [x] 2.4 Add fixture declarations for Vec.

## 3. Unification And Refinement

- [x] 3.1 Implement first-order constructor/index unification.
- [x] 3.2 Add occurs-check diagnostics for invalid metavariable solutions.
- [x] 3.3 Add impossible-branch detection from failed index unification.
- [x] 3.4 Add branch context refinement summaries for diagnostics and tests.

## 4. Clause Elaboration

- [x] 4.1 Represent source pattern clauses with stable spans.
- [x] 4.2 Elaborate constructor patterns by extending the context with constructor fields.
- [x] 4.3 Specialize branch expected types after index refinement.
- [x] 4.4 Emit deterministic case-tree artifacts.

## 5. Coverage

- [x] 5.1 Implement a constructor-split coverage worklist for the first fragment.
- [x] 5.2 Report missing branches with family/index display text.
- [x] 5.3 Report redundant branches when an earlier clause already covers the same refined space.
- [x] 5.4 Preserve source span information in coverage diagnostics.

## 6. Validation

- [x] 6.1 Add accepted Vec `head` fixture where `Nil` is impossible.
- [x] 6.2 Add accepted Vec `append` fixture if transparent Nat addition exists.
- [x] 6.3 Add rejected fixture for a checker profile without dependent-pattern support.
- [x] 6.4 Add rejected fixture for an unsupported equality-pattern case.
- [x] 6.5 Run focused MLTT/dependent-pattern tests and existing basic checker tests.

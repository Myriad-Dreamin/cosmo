## Why

Cosmo's eventual dependent pattern matching should refine indexed types such as
`Vec[A, n]` without forcing every type checker profile to implement that feature.
The language needs an elaboration proposal that can sit on top of MLTT-style core
metadata and remain optional for simpler checker profiles.

## What Changes

- Define dependent pattern matching as an elaboration capability over typed core
  declarations, constructor metadata, unification, and coverage checking.
- Require checker profiles to declare whether they support dependent patterns,
  including `cosmo0.subset` declaring support.
- Specify that unsupported dependent patterns are valid rejection results for
  profiles that do not implement the capability.
- Define the first implementation scope: constructor pattern elaboration,
  index refinement, impossible-branch detection, deterministic case trees, and
  coverage diagnostics.
- Defer advanced features such as with-abstraction, equality pattern matching,
  views, pattern synonyms, and without-K proof obligations.

## Capabilities

### New Capabilities

- `cosmo-dependent-pattern-elaboration`: Defines dependent pattern matching
  elaboration over MLTT-style core metadata, including constructor refinement,
  impossible cases, coverage, diagnostics, and profile gating.

### Modified Capabilities

None.

## Impact

- Adds a planning layer for dependent pattern matching while keeping support
  explicit in checker profile metadata.
- Future implementation will affect parser AST pattern metadata, MLTT declaration
  metadata, case-tree generation, coverage checking, and diagnostics.
- Simpler checker profiles may continue to reject dependent patterns explicitly,
  while `cosmo0.subset` declares support.

## Why

Cosmo needs room to experiment with multiple type-checking strategies without
forcing every checker to support the full language at once. The current
`packages/cosmoc/src/types` path is useful for the parser-source subset, but the
next stages need an explicit checker boundary so cosmo0, cosmoc, and future MLTT
experiments can coexist.

## What Changes

- Define a modular type checker architecture with explicit checker identities,
  feature profiles, inputs, outputs, diagnostics, and unsupported-feature
  behavior.
- Introduce a shared checker result shape that can represent success, ordinary
  type errors, unsupported features, internal checker limitations, and lowered
  typed artifacts.
- Require each checker to declare the language features it supports rather than
  silently accepting or partially checking unsupported source.
- Keep the existing basic expression checker as the first "simple" checker
  profile while allowing future MLTT, trait-aware, effect-aware, and dependent
  pattern checkers to be added in parallel.
- Define how cosmo0 and `packages/cosmoc` can select a checker profile for a
  package, module, test fixture, or experiment.

## Capabilities

### New Capabilities

- `cosmo-type-checker-experiments`: Defines the modular checker boundary,
  feature profile model, selection rules, diagnostics, and staged
  implementation expectations for cosmo0 and cosmoc.

### Modified Capabilities

None.

## Impact

- Adds OpenSpec requirements for checker modularity without changing existing
  compiler behavior immediately.
- Future implementation will affect `packages/cosmoc/src/types`, the cosmo0
  Scala type-checking path, package metadata validation, and type-checking tests.
- The existing parser-subset checker can remain in place, but it should be
  wrapped as one checker profile instead of being treated as the only checking
  architecture.

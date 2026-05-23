## Why

Cosmo needs a small dependent type-checking experiment that can support future
dependent pattern matching without turning every checker into a full proof
assistant. An MLTT-oriented core gives the project a principled place to model
universes, dependent functions, inductive families, equality, conversion, and
elaboration while preserving the existing staged bootstrap path.

## What Changes

- Introduce an experimental `mltt.core` checker profile built around a small
  Martin-Lof type theory core.
- Define a bidirectional checker architecture with separate checking,
  inference, conversion, first-stage normalization, metavariable, and diagnostic
  layers.
- Define MLTT core data for universes, Pi types, Sigma types, variables, lambdas,
  applications, lets, inductive-family references, constructors, equality, and
  neutral terms.
- Define implementation guidance for cosmo0 and `packages/cosmoc`, with the
  initial implementation scoped to a small and well-commented core rather than
  full language support.
- Scope the first conversion implementation to a declared WHNF-style
  normalization strategy and defer other normalization techniques to later
  research and separate proposals.
- Add MLTT type-checking guidance documentation covering concepts, implementation
  trade-offs, feature boundaries, and future theory discussion points.

## Capabilities

### New Capabilities

- `cosmo-mltt-core`: Defines the MLTT core term/type model, universe policy,
  purity boundary, and feature exclusions for the first MLTT checker.
- `cosmo-mltt-bidirectional-checker`: Defines bidirectional checking,
  conversion, normalization, diagnostics, and staged implementation behavior for
  the MLTT profile.

### Modified Capabilities

None.

## Impact

- Adds OpenSpec requirements and design notes for a new checker profile; it does
  not replace the current `packages/cosmoc/src/types` checker.
- Future implementation will add MLTT-oriented modules under `packages/cosmoc`
  and a corresponding cosmo0-side experiment or conformance harness.
- The first MLTT profile may reject traits, effects, async, object dispatch, C++
  imports, general recursion, and dependent pattern matching until later
  profiles add those capabilities.
- The first MLTT profile does not claim conformance to any advanced
  normalization strategy beyond its explicitly declared first-stage conversion
  behavior.

## Why

Stage 1 diagnostics and lexing need small typed collections and fallible results. `Option<T>`, `Result<T, E>`, and `Vec<T>` should be core0/std APIs even if their initial lowering still reuses existing descriptor infrastructure internally.

## What Changes

- Define a minimal standard API for `Option<T>`, `Result<T, E>`, and `Vec<T>`.
- Keep the public API in std and document any temporary descriptor-backed implementation as transitional.
- Add cosmo1 diagnostic lists and lexer token buffers that use these types.
- Add tests for construction, access, matching, mutation, and Stage 1 component usage.

## Capabilities

### New Capabilities

- `core0-option-result-vec-minimal`: Provides the Stage 1 collection/result API surface.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds or satisfies the `core0.option-result-vec` capability.

## Impact

- Unblocks `driver/diagnostic.cos` and token-buffer-oriented `lex/lexer.cos`.
- Establishes std ownership for common generic APIs.
- Keeps future user-defined generics out of scope.

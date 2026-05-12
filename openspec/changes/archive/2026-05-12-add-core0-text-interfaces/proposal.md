## Why

Stage 1 source, spans, diagnostics, and lexing need text operations. These APIs should be standard interfaces or modules, not descriptors, so cosmo1 can use a stable std surface while implementations evolve.

## What Changes

- Add core0 text interfaces for owned strings, text views or slices, length, byte/char access, and builder-like construction.
- Keep implementation details source-compiled where possible or extern/backend-backed behind std APIs.
- Add cosmo1 `source/source.cos`, `source/source_map.cos`, and text helpers.
- Add tests proving text APIs are available through std capabilities rather than descriptor metadata.

## Capabilities

### New Capabilities

- `core0-text-interfaces`: Provides Stage 1 text APIs for source and diagnostic code.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds or satisfies the `core0.text` capability.

## Impact

- Unblocks source text storage and slicing for cosmo1 Stage 1.
- Establishes the pattern for std API surface with implementation behind the boundary.
- Keeps `StringBuilder` and text operations out of the descriptor registry.

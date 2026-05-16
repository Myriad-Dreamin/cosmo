## Why

After diagnostics, hover is the smallest semantic query that makes the language server feel interactive. Cosmo needs a focused hover slice that can map positions to declarations or inferred types without dragging in broader navigation or completion work.

## What Changes

- Add hover lookup and rendering in `packages/cosmos` over syntax, name-resolution, and type information.
- Return generated LSP hover content and ranges for supported positions.
- Keep go-to-definition, references, completion, and semantic tokens out of this slice.

## Capabilities

### New Capabilities

- `cosmos-hover`: Defines position-based hover lookup and rendering for the initial Cosmo language-server slice.

### Modified Capabilities

- `cosmos-workspace-and-documents`: Supplies document snapshots and package sessions consumed by hover.
- `cosmos-diagnostics-pipeline`: Supplies the first stable analysis path reused by hover.

## Impact

- Future implementation will extend `packages/cosmos/` with position lookup, semantic rendering, and hover request handling.
- Gives the initial server a focused semantic query without broadening into the full editor feature surface.
- Implementation should land after `add-cosmos-diagnostics-pipeline` and before `integrate-cosmos-with-vscode`.

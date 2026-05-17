## Why

Diagnostics are the first user-visible outcome of a language server. `packages/cosmoc` already has parser, package, and checker diagnostics, but `packages/cosmos` does not yet convert them into LSP-facing results or publish them on document changes.

## What Changes

- Add a diagnostics pipeline in `packages/cosmos` that drives `packages/cosmoc` analysis for open documents and affected packages.
- Convert source spans, codes, severities, and messages into the generated LSP diagnostic shapes.
- Define a first refresh model for open, change, and close events, while leaving hover and other semantic queries to later proposals.

## Capabilities

### New Capabilities

- `cosmos-diagnostics-pipeline`: Defines LSP-facing diagnostics generation and publication from `packages/cosmoc` analysis results.

### Modified Capabilities

- `cosmos-workspace-and-documents`: Supplies document snapshots and package sessions consumed by diagnostics.
- `ls-base-lsp-lifecycle`: Supplies `publishDiagnostics` routing.

## Impact

- Future implementation will extend `packages/cosmos/` and connect existing `packages/cosmoc/` diagnostics to LSP outputs.
- Delivers the first directly useful editor feature for the Cosmo language server.
- Implementation should land after `create-cosmos-workspace-and-documents` and before `add-cosmos-hover`.

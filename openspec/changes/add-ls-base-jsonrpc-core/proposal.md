## Why

LSP is layered on JSON-RPC 2.0. Cosmo needs a transport-agnostic core for message envelopes, correlation, and codec behavior before it can add server lifecycle or editor-facing features.

## What Changes

- Add `packages/ls-base` with JSON-RPC message, id, and error models.
- Add deterministic request, response, and notification codecs plus pending-request tracking.
- Keep the first slice transport-agnostic and in-memory; stdio or editor process wiring stays out of scope.

## Capabilities

### New Capabilities

- `ls-base-jsonrpc-core`: Defines the reusable JSON-RPC core consumed by later language-server layers.

### Modified Capabilities

None.

## Impact

- Future implementation will add `packages/ls-base/` with JSON-RPC models, codecs, and session core helpers.
- Becomes the protocol base for later LSP lifecycle and `packages/cosmos` server work.
- Implementation should land after `add-lsp-types-generator-core-subset` and before `add-ls-base-lsp-lifecycle`.

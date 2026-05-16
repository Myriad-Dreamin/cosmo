## Why

JSON-RPC envelopes alone do not define a usable language server. Cosmo needs a typed LSP lifecycle layer for initialization, shutdown, document routing, and server capability declaration before `packages/cosmos` can serve editors.

## What Changes

- Extend `packages/ls-base` with typed LSP lifecycle handling over the JSON-RPC core.
- Add `initialize`, `initialized`, `shutdown`, and `exit` flow plus capability declaration and state transitions.
- Add typed routing for core text-document notifications and `publishDiagnostics`, while leaving transport process wiring for later.

## Capabilities

### New Capabilities

- `ls-base-lsp-lifecycle`: Defines typed LSP lifecycle and document-routing behavior on top of the JSON-RPC core.

### Modified Capabilities

- `ls-base-jsonrpc-core`: Supplies the message and session primitives used by the lifecycle layer.

## Impact

- Future implementation will extend `packages/ls-base/` with lifecycle state, handlers, and document notification routing.
- Establishes the protocol boundary consumed by the `packages/cosmos` server and VSCode integration.
- Implementation should land after `add-ls-base-jsonrpc-core` and before `create-cosmos-workspace-and-documents`.

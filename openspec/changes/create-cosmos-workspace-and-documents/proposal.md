## Why

A usable language server needs a stable model for workspace roots, package sessions, URIs, and open document snapshots before it can add diagnostics or semantic queries. Those boundaries belong in `packages/cosmos`, not in the editor client.

## What Changes

- Add `packages/cosmos` with workspace, package-session, URI, and open-document infrastructure.
- Define how document URIs map to package roots, module paths, and cached analysis sessions.
- Keep diagnostics, hover, and other semantic queries out of this slice so the package boundary stays focused.

## Capabilities

### New Capabilities

- `cosmos-workspace-and-documents`: Defines the `packages/cosmos` workspace and document-state foundation for later language-server features.

### Modified Capabilities

- `ls-base-lsp-lifecycle`: Supplies lifecycle and document routing consumed by `packages/cosmos`.
- `uri-sys`: Supplies stable URI parsing and normalization for document and workspace state.

## Impact

- Future implementation will add `packages/cosmos/` and its initial workspace and document modules.
- Establishes the package boundary shared by diagnostics, hover, and editor integrations.
- Implementation should land after `add-ls-base-lsp-lifecycle` and `add-uri-sys`, and before `add-cosmos-diagnostics-pipeline`.

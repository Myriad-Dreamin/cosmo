## Why

The repository already has a VSCode extension shell, but it does not yet launch a Cosmo-authored language-server stack with real diagnostics and hover behavior. A dedicated integration slice is needed so editor wiring does not leak back into core server packages.

## What Changes

- Update `editors/vscode` to launch the `packages/cosmos` server through a host wrapper.
- Wire document synchronization, diagnostics, and hover through the extension client.
- Add build, package, and smoke-test coverage for the first VSCode integration slice.

## Capabilities

### New Capabilities

- `cosmos-vscode-integration`: Defines the first VSCode integration path for the Cosmo-authored language server.

### Modified Capabilities

- `ls-base-lsp-lifecycle`: Supplies the client and server lifecycle contract consumed by the extension.
- `cosmos-diagnostics-pipeline`: Supplies diagnostics shown in the editor.
- `cosmos-hover`: Supplies hover results shown in the editor.

## Impact

- Future implementation will touch `editors/vscode/`, host wrapper code, build scripts, and editor smoke tests.
- Keeps process and editor wiring out of the core server packages while making the first end-to-end workflow usable.
- Implementation should land after `add-cosmos-hover`.

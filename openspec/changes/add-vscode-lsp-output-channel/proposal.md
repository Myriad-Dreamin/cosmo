## Why

There is no obvious VSCode output channel for the Cosmos language server's stderr. That makes it hard to inspect server startup, probe failures, runtime stderr, and diagnostic-refresh behavior such as why pressing `Ctrl+S` changes the diagnostics state.

## What Changes

- Add a named VSCode output channel for the Cosmo language server.
- Route language-server startup/probe failures and server stderr into that channel.
- Add a command that reveals the output channel from VSCode.
- Add tests or extension-level smoke coverage for output-channel creation and stderr capture.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `cosmos-vscode-integration`: Adds observable language-server output and debugging support to the VSCode integration.

## Impact

- Future implementation will touch `editors/vscode/src/lsp.ts`, possibly `editors/vscode/src/cosmos-host.ts`, and `editors/vscode/package.json` command contributions.
- The channel should help debug diagnostics refresh and host crashes without changing language semantics.
- Care should be taken not to mix LSP stdout protocol traffic into the human-readable output channel.

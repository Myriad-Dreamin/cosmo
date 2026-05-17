## Why

There is no obvious VSCode output channel for the Cosmos language server's stderr. That makes it hard to inspect server startup, probe failures, runtime stderr, and diagnostic-refresh behavior such as why pressing `Ctrl+S` changes the diagnostics state.

## What Changes

- Add a named VSCode output channel for the Cosmo language server.
- Route language-server startup/probe failures and server stderr into that channel.
- Emit a startup metadata banner from the VSCode-launched host to stderr so it appears in the output channel immediately after startup.
- Include the git revision, relevant version values, and resolved non-secret configuration in that startup metadata.
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
- The startup metadata should make "which binary/config did VSCode launch?" answerable from the output channel without attaching a debugger.
- Care should be taken not to mix LSP stdout protocol traffic into the human-readable output channel; stdout remains reserved for JSON-RPC/LSP framing.
- The configuration snapshot should avoid dumping secrets, full environments, or unstable machine-local noise that does not help debug host startup.

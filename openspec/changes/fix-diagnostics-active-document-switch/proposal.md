## Why

Diagnostics currently appear stale when switching between open files: the visible diagnostics do not update to match the newly active document, and pressing `Ctrl+S` can make diagnostics disappear. Diagnostics should be tied to the active document URI and current in-memory text, not to save events or whichever file most recently triggered a publish.

## What Changes

- Add an active-editor diagnostics refresh path in the VSCode integration.
- Keep Cosmos diagnostic state keyed by document URI so refreshing one file does not leak or clear diagnostics for another.
- Ensure save-only events do not change diagnostics for unchanged text.
- Add host/client smoke coverage with two open documents and an active-document switch.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `cosmos-vscode-integration`: Adds active-document diagnostic refresh behavior in the editor integration.
- `cosmos-diagnostics-pipeline`: Tightens per-URI diagnostic publication and refresh semantics.

## Impact

- Future implementation will touch `editors/vscode/src/lsp.ts`, host smoke tests, and possibly Cosmos diagnostics state helpers.
- This proposal does not decide the HelloWorld false positives; it ensures the diagnostics shown belong to the active URI and update without saving.
- The implementation should be compatible with both publish diagnostics and `textDocument/diagnostic` pull requests if both are kept.

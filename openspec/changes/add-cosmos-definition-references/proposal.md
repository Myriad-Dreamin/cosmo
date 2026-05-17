## Why

Cosmos hover already performs position-based semantic lookup, but the language server does not expose go-to-definition or find-references. Editor users need those navigation features for local declarations, members, parameters, locals, functions, and supported types.

## What Changes

- Add Cosmos semantic navigation helpers for `textDocument/definition` and `textDocument/references`.
- Reuse the existing parser, name-resolution, declaration-resolution, and expression-checking path used by hover where possible.
- Extend `ls-base` capability declaration and request registration for definition and references providers.
- Wire the generated VSCode host and extension smoke tests so VSCode can send definition/reference requests to the server.

## Capabilities

### New Capabilities

- `cosmos-definition-references`: Defines initial position-based go-to-definition and references support for supported symbols in open document snapshots.

### Modified Capabilities

- `ls-base-lsp-lifecycle`: Advertises and registers definition/reference request support.
- `cosmos-vscode-integration`: Routes VSCode definition/reference requests to the Cosmos host.

## Impact

- Future implementation will add a Cosmos LSP navigation module and tests, likely alongside or shared with hover lookup helpers.
- Initial scope is deterministic navigation for supported symbols in the current open document snapshot; package-wide closed-file indexing can be proposed separately.
- The VSCode host wrapper will need `textDocument/definition` and `textDocument/references` dispatch and initialize capabilities.

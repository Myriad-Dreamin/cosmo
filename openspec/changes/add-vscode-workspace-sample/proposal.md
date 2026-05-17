## Why

The repository has runnable Cosmo samples, but there is no ready-to-open VSCode workspace that lets a user choose a small editor experience and immediately exercise the extension. Opening individual sample files leaves too much implicit setup around package roots, language activation, and which file should be used for the first smoke check.

## What Changes

- Add a checked-in VSCode workspace sample focused on the existing sample package and `samples/HelloWorld/main.cos`.
- Keep the sample workspace lightweight: it should reference existing sample files, avoid generated output directories, and activate the Cosmo extension through ordinary `.cos` documents.
- Add validation that the workspace file is well-formed and only references paths that exist in the repository.
- Document the workspace as the first manual editor smoke path for local extension testing.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `cosmos-vscode-integration`: Adds a concrete sample workspace entry point for manual VSCode language-server evaluation.

## Impact

- Future implementation will add a `.code-workspace` asset under the sample/editor area and small validation coverage.
- Makes manual VSCode testing repeatable without changing language-server semantics.
- Provides a stable sample target for diagnostics, hover, and later navigation proposals.

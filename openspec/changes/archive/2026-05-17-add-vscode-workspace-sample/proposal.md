## Why

The repository has runnable Cosmo samples, but it does not define concrete sample project layouts for editor workspace discovery. Users need small directories they can open in VSCode to test package-root selection, virtual workspace roots, and package-less single-file fallback without relying on a `.code-workspace` file.

## What Changes

- Add `samples/cosmo.json` as a virtual workspace root, mirroring Cargo's virtual workspace root pattern and allowing users to open `samples/` directly.
- Add `samples/workspaces/virtual-root` as a virtual root `cosmo.json` sample whose members are ordinary packages.
- Add `samples/workspaces/package` as an ordinary package `cosmo.json` sample.
- Add `samples/workspaces/single-file` as a package-less single-file sample that uses only standard library/prelude capabilities.
- Add validation that the sample project manifests are well-formed, member paths exist, ordinary package sources live under `src/`, and the single-file sample has no owning package manifest.
- Document the sample project workspaces as the manual editor smoke path for local extension testing.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `cosmos-workspace-and-documents`: Defines the provisional project structure rules used by sample workspace discovery.
- `cosmos-vscode-integration`: Adds concrete sample project directories for manual VSCode language-server evaluation.

## Impact

- Future implementation will add sample project manifests and small validation coverage.
- Makes manual VSCode testing repeatable without changing language-server semantics.
- Provides stable sample targets for diagnostics, hover, and later navigation proposals.
- Does not require or add a VSCode `.code-workspace` file; opening folders directly remains the intended path.

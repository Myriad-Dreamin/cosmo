## Why

`cmd/cosmo` can run a single input file, but proposal work and generator work need a package-aware host entrypoint that can execute package-owned tools without ad hoc shell glue.

## What Changes

- Add a package-aware CLI surface centered on `cosmo -p <package> run`.
- Define how the host locates a package root, resolves the runnable entrypoint, and forwards trailing arguments.
- Keep package execution a repository tooling feature; it does not change cosmo0 source semantics or stage capability profiles.

## Capabilities

### New Capabilities

- `cosmo-package-run-cli`: Defines package-aware host execution for repository tools and generators.

### Modified Capabilities

None.

## Impact

- Future implementation will touch `cmd/cosmo/`, package selection plumbing, and contributor docs.
- Gives later generator and language-server packages a stable invocation contract.
- Implementation should land before `add-lsp-types-generator-core-subset`.

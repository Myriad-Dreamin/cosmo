## Why

The official LSP metamodel is too large and change-prone to model by hand. Cosmo needs a deterministic generator that consumes the upstream full `lsprotocol` metamodel and emits the complete protocol type surface used by language-server work.

## What Changes

- Add `packages/lsp-types` with a Cosmo-owned full LSP metamodel generator pipeline.
- Download the upstream full metamodel through `ureq-sys` and keep that downloaded input ignored by git.
- Generate checked-in full-spec output split into `lspt`-style modules: `base`, `type_aliases`, `enums`, `structs`, `request`, and `notification`.

## Capabilities

### New Capabilities

- `lsp-types-generator-core-subset`: Defines deterministic Cosmo generation for the full LSP metamodel output surface.

### Modified Capabilities

None.

## Impact

- Future implementation will add `packages/lsp-types/`, generator sources, ignored downloaded metamodel input, and checked-in generated outputs.
- Establishes the type boundary consumed by later `ls-base` and `packages/cosmos` work.
- Implementation should land after `add-cosmo-package-run-cli` and `add-ureq-sys`, and before `add-ls-base-jsonrpc-core`.

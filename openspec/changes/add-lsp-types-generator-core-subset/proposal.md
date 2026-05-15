## Why

The official LSP metamodel is too large and change-prone to model by hand. Cosmo needs a deterministic generator that consumes the checked-in `lsprotocol` metamodel and emits the initial protocol types required by the first language-server slice.

## What Changes

- Add `packages/lsp-types` with a checked-in `lsprotocol` metamodel input and a Cosmo generator pipeline.
- Generate only the initial core session subset needed by server bootstrap: positions, ranges, locations, diagnostics, hover, initialize, shutdown, exit, and core text-document notifications.
- Keep the first generator slice deterministic and offline; network fetching and full protocol coverage stay out of scope.

## Capabilities

### New Capabilities

- `lsp-types-generator-core-subset`: Defines deterministic Cosmo generation for the initial LSP metamodel subset.

### Modified Capabilities

None.

## Impact

- Future implementation will add `packages/lsp-types/`, checked-in metamodel input, generator sources, and generated outputs.
- Establishes the type boundary consumed by later `ls-base` and `packages/cosmos` work.
- Implementation should land after `add-cosmo-package-run-cli` and `add-ureq-sys`, and before `add-ls-base-jsonrpc-core`.

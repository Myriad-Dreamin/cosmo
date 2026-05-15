## Why

cosmo1 needs a verified IR boundary before lowering can safely feed C++ emission. This change defines the IR data model, verifier, and debug rendering used by the parser self-compile lowering sequence.

## What Changes

- Add cosmo1 IR modules, declarations, functions, blocks, locals, values, operations, and terminators.
- Add an IR verifier for structural and type consistency.
- Add deterministic IR debug rendering for fixtures.

## Capabilities

### New Capabilities

- `cosmo1-ir-model-verifier`: Defines cosmo1 IR data, verification, and debug rendering.

### Modified Capabilities

None.

## Impact

- Future implementation will introduce IR modules under `packages/cosmoc/src/ir`.
- Lowering and C++ emission proposals depend on this IR contract.

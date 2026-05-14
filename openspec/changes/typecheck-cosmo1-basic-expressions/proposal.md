## Why

Once declaration signatures exist, cosmo1 can start checking simple expression bodies. This slice establishes basic expression typing before member access, method calls, and control flow are added.

## What Changes

- Type-check literals, local declarations, names, blocks, assignments, returns, arithmetic, comparison, equality, and simple mismatch diagnostics.
- Produce typed expression data for later lowering.
- Keep member lookup, method calls, and full control flow for later proposals.

## Capabilities

### New Capabilities

- `cosmo1-basic-expression-typechecking`: Defines basic expression type checking for parser self-compile.

### Modified Capabilities

None.

## Impact

- Future implementation will extend `packages/cosmoc/src/types/check.cos` or equivalent.
- Establishes typed expression data consumed by later checking and lowering.

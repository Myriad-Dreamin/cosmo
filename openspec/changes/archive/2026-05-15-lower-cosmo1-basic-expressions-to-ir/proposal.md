## Why

Basic typed expressions need deterministic IR lowering before parser-specific members and control flow are introduced. This change lowers the expression core used by later IR proposals.

## What Changes

- Lower literals, local declarations, names, assignment, returns, direct calls, arithmetic, comparison, equality, temporary locals, and block sequencing.
- Verify lowered IR with the cosmo1 IR verifier.
- Keep field/method lowering and control-flow lowering for later slices.

## Capabilities

### New Capabilities

- `cosmo1-basic-expression-lowering`: Defines lowering of basic typed expressions into cosmo1 IR.

### Modified Capabilities

None.

## Impact

- Future implementation will extend cosmo1 lowering modules.
- Provides reusable lowering utilities for member and control-flow slices.

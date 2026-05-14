## Why

The parser source relies on nested `if`/`else`, `while`, early returns, and short-circuit boolean operations. This change completes verified IR lowering for the whole `parser.cos` type-checked module.

## What Changes

- Lower `if`, `else`, `while`, nested returns, and short-circuit `and`/`or`.
- Generate deterministic block labels.
- Validate complete verified IR for `packages/cosmoc/src/parser.cos`.

## Capabilities

### New Capabilities

- `cosmo1-parser-control-flow-lowering`: Defines parser-source control-flow lowering and complete verified parser IR.

### Modified Capabilities

None.

## Impact

- Completes the lowering sequence needed before C++ emission.
- Does not introduce optimization, exception handling, or general pattern-match lowering.

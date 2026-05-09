## Why

Compiler implementation code depends on structured control flow, variants, and pattern matching. These constructs need an explicit lowering strategy into LIR blocks, branches, tags, and payload operations.

## What Changes

- Lower `if`/`else`, `loop`, `while`, restricted `for`, `break`, `continue`, and block expressions into LIR.
- Lower enum-style variant construction into explicit LIR representation.
- Lower match expressions into LIR control flow with tag checks, payload extraction, and wildcard handling.
- Ensure lowered control-flow output passes the LIR type checker.
- Add tests for control-flow and variant/match lowering.

## Capabilities

### New Capabilities

- `cosmo0-control-flow-lowering`: Defines lowering of structured control flow, variants, and match expressions into cosmo0 LIR.

### Modified Capabilities

None.

## Impact

- Extends LIR lowering in `packages/cosmo0`.
- Uses the existing typed expression and LIR checker contracts.
- Does not add descriptor-backed runtime or collection lowering.

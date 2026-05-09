## Why

After typed expressions and LIR exist, cosmo0 needs a first lowering path for declarations and ordinary function bodies. This establishes the basic bridge from typed source-level compiler code to verified LIR.

## What Changes

- Lower typed modules, classes, fields, functions, parameters, locals, assignments, direct calls, field access, and returns into LIR.
- Preserve source-level type information needed by LIR verification.
- Generate explicit LIR locals and operation sequences for ordinary function bodies.
- Run the LIR type checker over lowered output.
- Add tests that lower declaration and function examples from typed expressions into verified LIR.

## Capabilities

### New Capabilities

- `cosmo0-declaration-lowering`: Defines lowering from typed declarations and ordinary function bodies into cosmo0 LIR.

### Modified Capabilities

None.

## Impact

- Adds declaration and function lowering modules to `packages/cosmo0`.
- Depends on typed expressions, the LIR model, and the LIR type checker.
- Does not include complex control-flow, pattern matching, or descriptor runtime lowering.

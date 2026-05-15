## Why

The final type-checking slice must prove that cosmo1 can type-check complete `parser.cos` bodies. Control flow, nested scopes, early returns, and return compatibility are the remaining type-checking features needed by that source.

## What Changes

- Type-check `if`, `else`, `while`, nested block scopes, early returns, and function return compatibility.
- Add minimal never/unreachable handling needed for early return paths.
- Add complete `parser.cos` type-check acceptance fixtures.

## Capabilities

### New Capabilities

- `cosmo1-parser-control-flow-typechecking`: Defines parser-source control-flow type checking and complete `parser.cos` type-check acceptance.

### Modified Capabilities

None.

## Impact

- Completes the type-checking sequence for the parser self-compile source.
- Does not add match exhaustiveness, exceptions, closures, or full flow-sensitive typing.

## Why

`parser.cos` is currently usable as a cosmo0-compiled Boolean parser library, but cosmo1 needs native syntax output before later semantic stages can type-check or lower it. This change starts the self-compile path by making the parser produce structured syntax for the subset used by `packages/cosmoc/src/parser.cos`.

## What Changes

- Add a native parser output slice for the `parser.cos` acceptance subset.
- Replace Boolean-only parser acceptance as the self-compile milestone boundary with syntax-producing parser behavior.
- Keep the first syntax output focused on declarations, types, class members, blocks, calls, selection, assignment, `if`, `while`, and returns used by `parser.cos`.

## Capabilities

### New Capabilities

- `cosmo1-native-parser-ast-output`: Defines native cosmo1 parser output for the supported `parser.cos` source subset.

### Modified Capabilities

None.

## Impact

- Future implementation will touch `packages/cosmoc/src/parser.cos` or a successor syntax parser module.
- Does not require full Cosmo parsing, parser recovery, staging, traits, user generics, or macro syntax.

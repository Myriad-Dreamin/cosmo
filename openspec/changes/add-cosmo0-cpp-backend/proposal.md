## Why

The cosmo0 pipeline needs a backend that turns verified LIR into deterministic C++ output. Keeping the backend behind the LIR contract prevents parser, typing, and lowering concerns from leaking into code generation.

## What Changes

- Add a C++ backend that accepts verified cosmo0 LIR.
- Emit C++ types, declarations, functions, classes, variants, blocks, operations, and control flow.
- Emit descriptor-backed runtime includes or support code required by lowered LIR operations.
- Apply stable naming, namespace, and output ordering rules.
- Add tests for deterministic C++ emission from equivalent LIR inputs.
- Add `packages/cosmoc/src/parser.cos` as an initial library-style compile target with no `main` entry point.
- Add `samples/HelloWorld/main.cos` as an initial executable compile target.

## Capabilities

### New Capabilities

- `cosmo0-cpp-backend`: Defines deterministic C++ code generation from verified cosmo0 LIR.

### Modified Capabilities

None.

## Impact

- Adds backend modules to `packages/cosmo0`.
- Produces C++ text from verified LIR.
- Adds compile validation coverage for `packages/cosmoc/src/parser.cos` and `samples/HelloWorld/main.cos`.
- Does not change the existing full Cosmo backend.

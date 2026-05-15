## Why

Verified parser IR only proves the middle pipeline. The self-compile milestone is complete when a cosmo0-compiled cosmo1 pipeline emits deterministic C++ for `parser.cos` and the configured C++ toolchain accepts it.

## What Changes

- Add deterministic C++ emission from verified parser IR.
- Emit parser classes, fields, methods, functions, references, control flow, primitive operations, and string runtime calls.
- Add end-to-end parser self-compile validation and repeated-output determinism checks.

## Capabilities

### New Capabilities

- `cosmo1-parser-self-compile-cpp-emission`: Defines C++ emission and end-to-end validation for parser self-compile.

### Modified Capabilities

None.

## Impact

- Future implementation will add cosmo1 C++ emission modules.
- This proposal completes the first parser self-compile milestone, but not full compiler self-hosting or linker integration.

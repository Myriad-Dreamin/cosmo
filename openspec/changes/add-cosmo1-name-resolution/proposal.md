## Why

Type checking `parser.cos` requires every name, field, method, parameter, local, and supported qualified path to resolve to a stable definition. This change adds the name resolution layer consumed by the type-checking sequence.

## What Changes

- Add symbol interning and definition ids.
- Add scopes for modules, classes, functions, blocks, parameters, and locals.
- Resolve parser-source names, fields, methods, and supported qualified paths.
- Report duplicate and unresolved-name diagnostics.

## Capabilities

### New Capabilities

- `cosmo1-name-resolution`: Defines symbol interning, scopes, definition collection, and name resolution for parser self-compile.

### Modified Capabilities

None.

## Impact

- Future implementation will extend `packages/cosmoc/src/names`.
- Establishes the resolved identifiers consumed by type checking and lowering.

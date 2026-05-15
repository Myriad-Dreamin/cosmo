## Why

After the IR model exists, cosmo1 needs to lower typed declarations and signatures before expression bodies can be lowered. This change maps parser-source classes, fields, methods, functions, parameters, receivers, and return types into IR declarations.

## What Changes

- Lower typed class and field declarations to IR type declarations.
- Lower top-level functions and methods to IR function declarations.
- Preserve receiver metadata, parameter order, return types, and stable symbols.

## Capabilities

### New Capabilities

- `cosmo1-declaration-lowering`: Defines lowering from typed declarations to cosmo1 IR declarations.

### Modified Capabilities

None.

## Impact

- Future implementation will extend `packages/cosmoc/src/ir/lower.cos` or equivalent.
- Function bodies may remain placeholder IR until later lowering proposals.

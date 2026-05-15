## Why

After the type model exists, cosmo1 must resolve declarations and explicit type expressions before checking function bodies. This change creates the semantic signature layer for the `parser.cos` self-compile target.

## What Changes

- Resolve top-level declarations, class members, fields, methods, aliases, parameters, and return annotations.
- Resolve primitive, user, function, and reference type expressions used by `parser.cos`.
- Validate `self` receiver forms before body checking.

## Capabilities

### New Capabilities

- `cosmo1-declaration-type-resolution`: Defines declaration and type-expression resolution for parser self-compile.

### Modified Capabilities

None.

## Impact

- Future implementation will extend `packages/cosmoc/src/types` and consume name-resolution output.
- Function bodies remain out of scope for this slice.

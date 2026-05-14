## Why

Syntax output needs stable identities and spans before name resolution, type checking, diagnostics, and lowering can share a common representation. This change completes the arena-backed syntax model needed by the parser self-compile path.

## What Changes

- Add arena-backed syntax identifiers for declarations, expressions, type expressions, parameters, and class members.
- Attach source spans to syntax nodes needed by diagnostics.
- Define stable accessors and debug rendering for parser self-compile fixtures.

## Capabilities

### New Capabilities

- `cosmo1-syntax-arenas-spans`: Defines arena-backed syntax storage and span coverage for parser self-compile input.

### Modified Capabilities

None.

## Impact

- Future implementation will extend `packages/cosmoc/src/syntax/ast.cos` and related source/span helpers.
- Establishes the syntax contract consumed by resolution and type checking proposals.

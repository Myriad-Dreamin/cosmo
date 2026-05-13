## Why

Later cosmo1 stages need stable IDs for syntax, types, scopes, definitions, and IR. `Arena<T>` and `Id<T>` should be core0/std abstractions that preserve type safety without requiring user-defined generics.

## What Changes

- Add standard `Arena<T>` and `Id<T>` API surface.
- Keep these sealed standard generic applications in cosmo0.
- Add cosmo1 `syntax/ast` arena usage and prepare later `types`, `names`, and `ir` arena storage.
- Add tests for allocation, typed IDs, lookup, mutation boundaries, and invalid ID mixing.

## Capabilities

### New Capabilities

- `core0-arena-id`: Provides arena storage and typed ID support for later cosmo1 compiler data.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds the later-stage `core0.arena-id` capability.

## Impact

- Unblocks Stage 2 syntax AST storage.
- Prepares later type, name, and IR data structures.
- Keeps phantom typed IDs as sealed std types rather than user-defined generics.

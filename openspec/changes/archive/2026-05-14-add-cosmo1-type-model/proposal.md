## Why

cosmo1 needs its own type data before it can type-check `parser.cos` independently from the Scala cosmo0 typer. This change establishes the type representation used by later declaration and expression checking proposals.

## What Changes

- Add cosmo1-owned type ids and type storage.
- Represent primitives, user classes, function types, references, never/error types, and display names.
- Add type comparison and diagnostic foundations.

## Capabilities

### New Capabilities

- `cosmo1-type-model`: Defines cosmo1-owned type data required by parser self-compile type checking.

### Modified Capabilities

None.

## Impact

- Future implementation will introduce cosmo1 type modules under `packages/cosmoc/src/types`.
- Does not add full generic solving, trait constraints, or type-level evaluation.

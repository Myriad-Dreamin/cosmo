## Why

cosmo0 needs a source-level typed expression layer before lowering to a low-level IR. This layer is where names, source types, descriptor method signatures, references, mutation, and expression result types become explicit.

## What Changes

- Define the cosmo0 typed expression representation.
- Add source-level type definitions for scalar types, user declarations, aliases, references, and registered standard generic applications.
- Add a typer that converts untyped cosmo0 representation into typed expressions.
- Implement name resolution, local scopes, field lookup, call checking, assignment checking, return checking, and alias expansion at the source-expression level.
- Type-check descriptor-defined method surfaces needed by early standard generics.
- Emit source diagnostics for type errors without relying on the full Cosmo typer.

## Capabilities

### New Capabilities

- `cosmo0-typed-expression-typing`: Defines typed source expressions and the front-end typer that turns cosmo0 untyped representation into typed expression trees.

### Modified Capabilities

None.

## Impact

- Adds typed-expression and source-type modules to `packages/cosmo0`.
- Establishes the typed input consumed by LIR lowering.
- Does not reuse the existing full `Typer` pipeline as the cosmo0 source typer.

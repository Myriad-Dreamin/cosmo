## Why

cosmo0 needs a low-level IR that is smaller and more explicit than typed source expressions. Defining LIR separately gives lowering, verification, and C++ generation a stable contract that does not depend on full Cosmo IR concepts.

## What Changes

- Define cosmo0 LIR modules, declarations, functions, blocks, locals, operations, values, and terminators.
- Represent control flow with explicit blocks and branch terminators.
- Represent calls, field access, assignments, returns, variant data, and descriptor operations in low-level form.
- Add a debug rendering surface for LIR inspection and tests.
- Keep LIR independent of parser syntax and full-language IR nodes.

## Capabilities

### New Capabilities

- `cosmo0-lir-model`: Defines the cosmo0 low-level IR data model consumed by LIR checking, lowering, and C++ code generation.

### Modified Capabilities

None.

## Impact

- Adds LIR model modules to `packages/cosmo0`.
- Creates the common data contract for later lowering and backend work.
- Does not change parsing, source typing, or code generation behavior by itself.

## Why

LIR lowering needs a verifier so each lowering step can prove it produced well-formed low-level code. A dedicated LIR type checker keeps low-level invariants separate from the source-level typer.

## What Changes

- Add a type checker for cosmo0 LIR.
- Validate local definitions, local uses, operation result types, call signatures, intrinsic signatures, branch targets, terminators, and function return types.
- Validate reference and mutability invariants that must hold after lowering.
- Validate variant payload shapes and descriptor operation signatures in LIR form.
- Add hand-written LIR fixtures for positive and negative checker tests.

## Capabilities

### New Capabilities

- `cosmo0-lir-type-checking`: Defines the LIR well-formedness and type-checking contract for verified cosmo0 LIR.

### Modified Capabilities

None.

## Impact

- Adds LIR checking modules to `packages/cosmo0`.
- Gives subsequent LIR lowering work a validation target.
- Does not replace the source-level typed expression typer.

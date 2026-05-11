## Why

The shared parser accepts full Cosmo syntax, but cosmo0 needs a smaller and explicit source representation before type checking. An untyped expression representation and elaborator provide the first cosmo0-specific boundary between parser syntax and compiler semantics.

## What Changes

- Define the cosmo0 untyped declaration, statement, expression, and type representation.
- Add an elaborator from shared parser syntax into the cosmo0 untyped representation.
- Normalize parser-level surface forms into cosmo0-specific constructs.
- Reject unsupported full-language constructs before they enter later cosmo0 phases.
- Preserve source spans for accepted and rejected constructs.
- Add tests for accepted core forms and rejected full-language forms.

## Capabilities

### New Capabilities

- `cosmo0-untyped-repr-elaboration`: Defines the parser-to-cosmo0 elaboration boundary and the untyped representation used by later cosmo0 phases.

### Modified Capabilities

None.

## Impact

- Adds new untyped representation and elaborator modules to `packages/cosmo0`.
- Uses the existing parser as input but does not alter parser behavior.
- Provides the first executable subset boundary for cosmo0.

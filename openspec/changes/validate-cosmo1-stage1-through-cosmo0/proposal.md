## Why

cosmo0 should be validated against a realistic first cosmo1 stage, not only isolated unit fixtures. A Stage 1 validation target proves the pipeline can accept compiler-shaped source and emit backend output for an early bootstrap milestone.

## What Changes

- Define the first cosmo1 Stage 1 validation target for source loading, spans, diagnostics, tokens, lexing-oriented structures, and text output.
- Define the primitive descriptor set and core0 standard capability set required by that Stage 1 target.
- Add cosmo1-style Stage 1 source that uses only the accepted cosmo0 subset and implemented primitive descriptors or staged standard capabilities.
- Add package-level check validation for the Stage 1 source.
- Add package-level compile validation through the cosmo0 C++ backend.
- Add negative validation cases for accidental full-language features in Stage 1 source.
- Document the Stage 1 cosmo0 validation workflow.

## Capabilities

### New Capabilities

- `cosmo1-stage1-cosmo0-validation`: Defines validation of the first cosmo1-style stage through cosmo0 check and compile flows.

### Modified Capabilities

None.

## Impact

- Adds cosmo1 Stage 1 validation sources and tests.
- Exercises the cosmo0 package pipeline, staged standard capability validation, primitive descriptor lowering, LIR lowering, and C++ backend together.
- Does not implement the full cosmo1 compiler.

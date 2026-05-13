## Why

After the Stage 1 primitive and std capabilities are available, cosmo0 needs a realistic cosmo1 package validation target that proves the components work together.

## What Changes

- Wire the Stage 1 package layout, allowed cosmo0 feature set, and required capability profile.
- Complete the Stage 1 source slice for source text, spans, source maps, diagnostics, tokens, and lexing.
- Add package-level check and compile validation.
- Add deterministic output and negative unsupported-feature fixtures.

## Capabilities

### New Capabilities

- `cosmo1-stage1-capability-profile`: Defines the concrete Stage 1 validation profile and package slice.

### Modified Capabilities

- `cosmo1-stage1-cosmo0-validation`: Uses the primitive descriptor and core0/std capability model.

## Impact

- Gives cosmo0 a compiler-shaped Stage 1 validation target.
- Proves the descriptor/std capability plan through real cosmo1 source.
- Does not implement later parser, JSON loader, name resolution, or type checking stages.

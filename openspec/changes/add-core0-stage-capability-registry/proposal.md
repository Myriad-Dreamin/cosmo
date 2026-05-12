## Why

cosmo1 stages need different primitive and standard capabilities. Stage validation should use named capability profiles instead of a descriptor-only set, so later capabilities do not block earlier stages.

## What Changes

- Add core0 standard capability identifiers.
- Add stage profiles for cosmo0/cosmo1 validation requests.
- Validate required primitive descriptors, standard modules, and extern runtime bindings for a requested stage.
- Add Stage 1 package metadata/profile scaffolding for the existing `packages/cosmoc` compiler package.

## Capabilities

### New Capabilities

- `core0-stage-capability-registry`: Defines named primitive/std capability profiles for staged validation.

### Modified Capabilities

- `cosmo0-staged-runtime-capabilities`: Uses capability profiles for stage checks.
- `cosmo1-stage1-cosmo0-validation`: Consumes the Stage 1 profile.

## Impact

- Makes `validate-cosmo1-stage1-through-cosmo0` task 1.1 concrete.
- Adds missing-capability diagnostics.
- Prevents JSON, command, arena, map/set, and big-number gaps from blocking Stage 1.

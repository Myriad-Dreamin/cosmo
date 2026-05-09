## Why

cosmo1 stages require runtime-backed facilities at different times, and early stages should not be blocked by later capabilities. A staged runtime capability model lets cosmo0 validate only the descriptors required by the current stage.

## What Changes

- Add staged descriptor availability for runtime-backed capabilities.
- Add JSON value and parse bridge descriptors for metadata and parser-facing stages.
- Add path and filesystem descriptors for source loading and generated output writing.
- Add command execution as an optional later-stage descriptor.
- Add lossless numeric literal representation support for stages that need literal preservation before arbitrary-precision arithmetic.
- Add validation for stage-required descriptor sets.

## Capabilities

### New Capabilities

- `cosmo0-staged-runtime-capabilities`: Defines staged availability and validation of runtime descriptors used by cosmo0-compiled compiler stages.

### Modified Capabilities

None.

## Impact

- Extends the descriptor registry and validation model in `packages/cosmo0`.
- Adds runtime-capability tests for available and unavailable descriptor sets.
- Does not require every runtime descriptor to be implemented before earlier stages can be checked.

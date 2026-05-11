## Why

Later cosmo1 driver and link stages need to invoke external tools such as C++ compilers, linkers, or test programs. Command execution is a later-stage std module backed by runtime bindings, not a Stage 1 requirement and not a descriptor family.

## What Changes

- Add standard command construction and execution APIs.
- Bind command execution through trusted extern runtime hooks.
- Add capability validation so command support is only required by later stages.
- Add cosmo1 `link/command.cos` and driver smoke usage.

## Capabilities

### New Capabilities

- `core0-command`: Provides later-stage command execution APIs.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds the later-stage `core0.command` capability.
- `cosmo0-extern-abi-hooks`: Supplies runtime bindings behind command APIs.

## Impact

- Unblocks later build/link/run integration.
- Keeps Stage 1 independent of command execution.
- Keeps process execution out of the descriptor registry.

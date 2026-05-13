## Why

Stage 1 must load source files. Path and filesystem APIs should be standard modules with extern-backed implementation where needed, not descriptor families.

## What Changes

- Add minimal `Path`, `IoError`, and `Fs.read_to_string` std APIs.
- Optionally include `Fs.write_string` only if it remains inside the focused source-loading/filesystem boundary.
- Bind host implementation through extern ABI hooks.
- Add cosmo1 source loading and input path handling.

## Capabilities

### New Capabilities

- `core0-path-fs`: Provides Stage 1 source-loading filesystem APIs.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds or satisfies the `core0.path-fs` capability.
- `cosmo0-extern-abi-hooks`: Supplies runtime bindings behind the std APIs.

## Impact

- Unblocks `source/source.cos` source loading by path.
- Unblocks early `driver/config.cos` input path handling.
- Keeps filesystem support outside the descriptor registry.

## Why

Core0 standard APIs sometimes need host or runtime implementation support. cosmo0 needs a minimal extern ABI hook so std modules can bind to runtime symbols without making each runtime facility a descriptor family.

## What Changes

- Add metadata for extern runtime bindings used by core0/std modules.
- Track backend runtime symbols, includes, or support libraries independently from descriptor operations.
- Add diagnostics for missing or unsupported extern runtime bindings.
- Add a small cosmo1 smoke component that calls an extern-backed std function through the standard API.

## Capabilities

### New Capabilities

- `cosmo0-extern-abi-hooks`: Defines minimal extern/runtime binding support for core0 standard modules.

### Modified Capabilities

- `cosmo0-staged-runtime-capabilities`: Allows stage capabilities to require extern runtime bindings behind std APIs.

## Impact

- Keeps filesystem, output, JSON, command, and later runtime-backed APIs out of the descriptor registry.
- Gives the backend a standard way to record runtime requirements.
- Unblocks later path/filesystem and text-output proposals.

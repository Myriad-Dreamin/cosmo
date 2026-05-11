## Why

Stage 2 needs to load parser JSON while the native parser matures. JSON support should be a core0/std bridge with extern-backed parsing, not a descriptor family.

## What Changes

- Add `JsonValue` and `Json.parse` standard APIs.
- Add object, array, string, boolean, null, and numeric accessors needed by selected parser JSON.
- Bind parser/runtime implementation through extern ABI hooks where needed.
- Add cosmo1 `syntax/json_loader` for selected sample AST JSON.

## Capabilities

### New Capabilities

- `core0-json-bridge`: Provides the transitional JSON bridge for syntax and metadata loading.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds the later-stage `core0.json` capability.

## Impact

- Unblocks Stage 2 JSON AST loading.
- Later supports package metadata and cache JSON.
- Keeps JSON out of the descriptor registry.

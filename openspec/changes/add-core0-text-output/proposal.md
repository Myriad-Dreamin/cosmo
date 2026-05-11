## Why

Stage 1 diagnostics and smoke tests need to render text. Output sinks should be standard APIs with backend or extern support behind them, while diagnostic formatting remains in cosmo1.

## What Changes

- Add minimal text output APIs such as `TextWriter`, `Stdout`, `Stderr`, or equivalent module functions.
- Keep formatting and diagnostic structure in cosmo1.
- Bind output to runtime support through extern ABI hooks where required.
- Add cosmo1 diagnostic rendering to an output sink.

## Capabilities

### New Capabilities

- `core0-text-output`: Provides standard text output sinks for Stage 1 diagnostics and smoke tests.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds or satisfies the `core0.text-output` capability.

## Impact

- Unblocks `driver/diagnostic.cos` rendering.
- Enables deterministic output tests for Stage 1 diagnostics.
- Keeps output support out of the descriptor registry.

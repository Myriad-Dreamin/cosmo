## Why

The lexer and parser need to preserve numeric literal text before arbitrary-precision arithmetic exists. Later type checking and evaluation need a standard big-number capability, but Stage 1 should not require it.

## What Changes

- Preserve raw numeric literal text in lexer/parser-facing data.
- Define when numeric text is parsed into primitive values versus preserved.
- Add later `BigInt` and `BigDecimal` std capability hooks without making them descriptors.
- Add cosmo1 numeric token and literal AST usage.

## Capabilities

### New Capabilities

- `core0-lossless-numeric-literals`: Defines lossless numeric literal preservation and later big-number std integration.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds the later-stage `core0.big-number` capability while keeping Stage 1 independent of it.

## Impact

- Improves Stage 1 numeric token correctness.
- Unblocks later parser literal AST and type-checking constants.
- Keeps `BigInt` and `BigDecimal` in std rather than descriptors.

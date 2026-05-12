## Why

Descriptors should be a small compiler escape hatch, not the default way to grow the standard library. cosmo0 needs a primitive descriptor boundary so high-level runtime APIs such as JSON, filesystem, commands, string builders, and big-number support move to core0/std interfaces instead of becoming Scala-side descriptor families.

## What Changes

- Define the primitive descriptor whitelist.
- Restrict descriptor-backed operations to compiler-essential primitives and intrinsics.
- Move ordinary runtime capability names out of the descriptor expansion path.
- Update cosmo0 specs for primitive types, expressions, runtime descriptors, and descriptor rejection rules.
- Add cosmo1 `source/span.cos` and basic `lex/token.cos` smoke components that rely only on primitive support.

## Capabilities

### New Capabilities

- `cosmo0-primitive-descriptor-boundary`: Defines which descriptor support is compiler intrinsic.

### Modified Capabilities

- `cosmo0-staged-runtime-capabilities`: Uses the primitive descriptor boundary as the descriptor side of each stage profile.

## Impact

- Narrows the long-term descriptor registry.
- Adds negative tests for attempting to model ordinary std APIs as descriptors.
- Unblocks source spans and basic token definitions for cosmo1 Stage 1.

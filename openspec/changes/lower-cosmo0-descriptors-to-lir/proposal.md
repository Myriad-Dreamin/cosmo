## Why

cosmo0 supports sealed standard generics and runtime facilities through descriptors rather than compiled standard-library source. These descriptor-backed operations need a lowering path into LIR intrinsics and runtime operations.

## What Changes

- Lower descriptor-backed operations for core standard generics such as `Vec`, `Option`, `Result`, `Arena`, `Id`, `Map`, and `Set`.
- Lower descriptor-backed scalar, string, and string-builder operations required by compiler code.
- Represent descriptor operations as explicit LIR intrinsics or runtime calls.
- Preserve deterministic collection iteration policy in lowered LIR.
- Ensure descriptor-lowered output passes the LIR type checker.
- Add tests for descriptor operation lowering and invalid descriptor usage.

## Capabilities

### New Capabilities

- `cosmo0-descriptor-lowering`: Defines lowering of standard generic and runtime descriptor operations into cosmo0 LIR.

### Modified Capabilities

None.

## Impact

- Extends LIR lowering in `packages/cosmo0`.
- Connects source-level descriptor typing to backend-visible LIR operations.
- Does not implement C++ emission for descriptor operations by itself.

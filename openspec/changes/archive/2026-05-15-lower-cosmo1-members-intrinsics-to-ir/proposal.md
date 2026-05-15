## Why

`parser.cos` cannot lower without field access, field assignment, method calls, receiver passing, `String` intrinsics, and primitive helper operations. This change adds the member and intrinsic lowering layer.

## What Changes

- Lower field get and field set operations.
- Lower resolved method calls with receiver arguments.
- Lower parser-required `String` intrinsics and primitive operations.
- Lower top-level helper calls used by `parser.cos`.

## Capabilities

### New Capabilities

- `cosmo1-member-intrinsic-lowering`: Defines lowering of member access, method calls, intrinsics, and helper calls into cosmo1 IR.

### Modified Capabilities

None.

## Impact

- Future implementation will extend lowering and IR operation coverage.
- Does not add trait dispatch, overload resolution, or arbitrary std APIs.

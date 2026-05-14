## Why

`parser.cos` is dominated by field access, method calls, receiver mutation, top-level helpers, and `String` intrinsics. This change completes the member and call rules needed before full parser-source type checking.

## What Changes

- Type-check field access and field assignment.
- Type-check method selection, method calls, top-level function calls, and receiver mutability.
- Add the `String` intrinsic signatures used by the parser source.
- Type-check boolean `and`/`or` expressions.

## Capabilities

### New Capabilities

- `cosmo1-member-call-typechecking`: Defines member, call, receiver, intrinsic, and boolean operator checking for parser self-compile.

### Modified Capabilities

None.

## Impact

- Future implementation will extend member lookup and call checking in cosmo1 type-checking modules.
- No general overload resolution or trait dispatch is introduced.

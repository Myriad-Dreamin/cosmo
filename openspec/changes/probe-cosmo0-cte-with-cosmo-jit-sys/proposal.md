## Why

After `cosmo-jit-sys` proves clang-repl execution, cosmo0 needs a small
end-to-end probe showing that the compiler can route a compile-time evaluation
request through that substrate. The probe should validate the integration
boundary without preempting the full macro system design.

## What Changes

- Add an experimental, gated cosmo0 compile-time evaluation probe that depends
  on `cosmo-jit-sys`.
- Recognize a deliberately tiny smoke input, `type x = 1 + 1`, and route its
  arithmetic through a structured JIT request.
- Assert that the probe resolves the alias to the computed value `2` and
  reports stable diagnostics when JIT execution is unavailable or fails.
- Keep general type-level arithmetic, macro provider execution, reflection,
  hygiene, generated declarations, and full macro output validation out of
  scope for this change.

## Capabilities

### New Capabilities

- `cosmo0-cte-jit-probe`: Defines the temporary, gated cosmo0 compile-time
  evaluation smoke integration through `cosmo-jit-sys`.

### Modified Capabilities

- None.

## Impact

- Depends on `add-cosmo-jit-sys-clang-repl-poc`.
- Adds a small cosmo0 compile/checker or driver integration point guarded as an
  experiment.
- Adds a focused test or fixture for `type x = 1 + 1`.
- Does not change the full `cosmo0-compile-time-evaluation` macro contract in
  `introduce-cosmo0-macro-system`.

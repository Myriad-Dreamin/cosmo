## Why

After the cosmo eval contract proves Clang-backed provider-entry compilation
with PCH/precompiled context reuse, cosmo0 needs a small end-to-end probe
showing that cosmo0 can route a compile-time evaluation request through its own
eval module. The probe should validate the integration boundary without
preempting the full macro system design.

## What Changes

- Add an experimental, gated cosmo0 compile-time evaluation probe that depends
  on cosmo0 eval mode.
- Recognize a deliberately tiny smoke input, `type x = 1 + 1`, and route its
  arithmetic through a structured `CosmoEvalRequest`.
- Compile a small provider entry function through cosmo0 eval rather than a
  clang-repl / clangInterpreter session.
- Assert that the probe resolves the alias to the computed value `2` and
  reports stable diagnostics when eval mode is disabled, unavailable, cannot
  compile the provider entry, or cannot execute the compiled entry.
- Keep general type-level arithmetic, macro provider execution, reflection,
  hygiene, generated declarations, and full macro output validation out of
  scope for this change.

## Capabilities

### New Capabilities

- `cosmo0-cte-compile-probe`: Defines the temporary, gated cosmo0 compile-time
  evaluation smoke integration through cosmo0 eval mode.

### Modified Capabilities

- None.

## Impact

- Depends on `add-cosmo-eval-pch-function-compile-poc`.
- Adds a small cosmo0 compile/checker or driver integration point guarded as an
  experiment.
- Adds a focused test or fixture for `type x = 1 + 1`.
- Does not change the full `cosmo0-compile-time-evaluation` macro contract in
  `introduce-cosmo0-macro-system`.

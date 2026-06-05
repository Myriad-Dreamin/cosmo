## Why

`docs/cosmo/compile-time-evaluation.typ` rejects the original clang-repl /
clangInterpreter direction because interpreter execution can diverge from
ordinary Clang compilation and may miscompile provider code. The repository
needs a native compile-time execution support component that keeps Clang as the
semantic source of truth while accelerating small provider-entry compilation
with PCH or an equivalent precompiled context.

## What Changes

- Add a native `cosmo-cte-sys` support component for compile-time execution.
- Compile explicit provider entry functions as ordinary Clang translation units
  instead of evaluating snippets through clang-repl or clangInterpreter.
- Add a precompiled context cache using PCH, Clang modules, module cache, or an
  equivalent Clang-owned precompiled representation.
- Reuse the repository LLVM/Clang distribution and CMake discovery conventions
  already used by `cosmo-clang-sys`, without requiring clang-repl artifacts.
- Add a proof-of-concept smoke path that compiles one provider entry function,
  loads or invokes it through the adapter, and returns a structured result.
- Add benchmark coverage for cold context creation, warm PCH/precompiled context
  reuse, first provider-entry compile, repeated provider-entry compile, and a
  heavy-header input that includes `nlohmann/json.hpp`.
- Keep macro expansion and full compile-time evaluation integration outside
  this change.

## Capabilities

### New Capabilities

- `cosmo-cte-sys`: Defines the native Clang compile-time execution support
  component, C ABI boundary, PCH/precompiled context cache, single provider-entry
  compile smoke path, and benchmark reporting for future compile-time
  evaluation consumers.

### Modified Capabilities

- None.

## Impact

- Adds a native support package under `packages/cosmo-cte-sys`.
- May move or share LLVM/Clang CMake helper files so `cosmo-clang-sys` and
  `cosmo-cte-sys` use the same manifest, cache, and toolchain options.
- Adds focused native smoke tests and benchmark scripts or test targets,
  including a benchmark shape for the repository nlohmann JSON header.
- Removes the requirement that the LLVM/Clang distribution include clang-repl.
- Establishes the dependency that later compile-time evaluation probes can use
  without adding the full macro system.

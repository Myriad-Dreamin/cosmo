## Why

`docs/cosmo/compile-time-evaluation.typ` makes `cosmo-jit-sys` the C++
compile-time execution substrate for later macro providers, but the repository
does not yet have that component or proof that the selected clang-repl artifact
can execute provider code in the local build pipeline.

## What Changes

- Add a native `cosmo-jit-sys` support component that owns clang-repl based
  execution behind a narrow C ABI.
- Reuse the repository LLVM/Clang distribution and CMake discovery conventions
  already used by `cosmo-clang-sys`, including the manifest component that
  supplies `clang-repl`.
- Add a clang-repl proof-of-concept smoke path that executes C++ code and
  returns a structured result instead of only proving that the binary exists.
- Add benchmark coverage for cold session startup, first evaluation, warm
  repeated evaluation, and a heavy header input that includes
  `nlohmann/json.hpp`, with machine/toolchain metadata recorded for comparison.
- Keep macro expansion and full compile-time evaluation integration outside
  this change.

## Capabilities

### New Capabilities

- `cosmo-jit-sys`: Defines the native clang-repl execution support component,
  C ABI boundary, smoke execution contract, and benchmark reporting for future
  compile-time evaluation consumers.

### Modified Capabilities

- None.

## Impact

- Adds a native support package under `packages/cosmo-jit-sys`.
- May move or share LLVM/Clang CMake helper files so `cosmo-clang-sys` and
  `cosmo-jit-sys` use the same manifest, cache, and toolchain options.
- Adds focused native smoke tests and benchmark scripts or test targets,
  including a benchmark shape for the repository nlohmann JSON header.
- Establishes the dependency that later compile-time evaluation probes can use
  without adding the full macro system.

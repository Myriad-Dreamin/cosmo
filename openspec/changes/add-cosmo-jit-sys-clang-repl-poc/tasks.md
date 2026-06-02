## 1. Native Package Setup

- [ ] 1.1 Add `packages/cosmo-jit-sys` with CMake project metadata, source,
  include, test, and benchmark directories.
- [ ] 1.2 Reuse or move the existing LLVM/Clang CMake helper so
  `cosmo-clang-sys` and `cosmo-jit-sys` share LLVM version, manifest, cache,
  offline, target platform, target architecture, and GNU toolchain options.
- [ ] 1.3 Add a `cosmoJit` native target and platform library artifact with
  position-independent code and the required Clang/LLVM include and link
  settings.
- [ ] 1.4 Add a clear configure-time diagnostic for missing clang-repl support.

## 2. C ABI And Session Model

- [ ] 2.1 Add `include/cosmo_jit_sys.h` with `cosmo_jit_sys_*` status values,
  string views, session options, snippet requests, result structs, opaque
  handles, and dispose functions.
- [ ] 2.2 Implement session creation and disposal without exposing Clang, LLVM,
  or clang-repl implementation types.
- [ ] 2.3 Implement snippet evaluation that returns status, diagnostics,
  captured output summary, integer smoke result, and generated artifact summary
  where available.
- [ ] 2.4 Add defensive validation for null pointers, empty snippets, missing
  entry points, and oversized smoke inputs.

## 3. clang-repl Proof Of Concept

- [ ] 3.1 Add a smoke runner or CTest target that evaluates an exported C++
  entry point computing `1 + 1` and asserts result `2`.
- [ ] 3.2 Add a second smoke snippet that includes a standard C++ header and
  instantiates a standard library type under clang-repl.
- [ ] 3.3 Document the smoke command, required LLVM environment variables, and
  expected failure message when clang-repl is unavailable.

## 4. Benchmark Coverage

- [ ] 4.1 Add a benchmark command for cold session startup, first snippet
  evaluation, and warm repeated evaluation.
- [ ] 4.2 Write benchmark reports under `target/cosmo/bench/cosmo-jit-sys/`
  with host, toolchain, build profile, backend, input shape, and timing data.
- [ ] 4.3 Add a heavy-header benchmark input that includes
  `nlohmann/json.hpp` from the repository JSON external include root,
  constructs or parses a `nlohmann::json` value, returns a deterministic scalar
  result, and records the include root in the report.
- [ ] 4.4 Keep benchmark pass/fail based on successful execution and report
  generation rather than uncalibrated absolute timing thresholds.

## 5. Validation

- [ ] 5.1 Run existing package-run and native CMake tests that cover
  `cosmo-clang-sys` after any shared CMake helper refactor.
- [ ] 5.2 Run the `cosmo-jit-sys` configure, build, smoke, and benchmark paths
  on a toolchain with clang-repl available.
- [ ] 5.3 Record any platform limitations or skipped clang-repl support in the
  change notes before handoff.

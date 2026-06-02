## Context

`cosmo-clang-sys` already integrates Clang through a CMake-built native support
library and uses `packages/cosmo-clang-sys/config/llvm-manifest.json` to fetch
or locate LLVM/Clang artifacts. That manifest already includes `clang-repl`
where `llvm-dist` publishes it. The compile-time evaluation design now needs a
separate component whose job is execution, not C++ namespace lookup.

This change is an implementation slice for proving the execution substrate. It
does not expose macro provider APIs and does not change accepted cosmo0 source
semantics.

## Goals / Non-Goals

**Goals:**

- Add `packages/cosmo-jit-sys` as the native component for clang-repl backed
  execution.
- Prove that the selected LLVM/Clang distribution can run clang-repl code in
  the repository build environment.
- Provide a C ABI that returns structured status, diagnostics, stdout/stderr
  summaries, and integer smoke results.
- Add benchmark output that can compare cold, warm, and heavy-header JIT costs
  across commits and toolchains.

**Non-Goals:**

- Implement full macro function input/output records.
- Evaluate arbitrary Cosmo expressions or type-level programs.
- Connect macro expansion, reflection, hygiene, or generated source validation.
- Promise stable absolute benchmark thresholds in CI.

## Decisions

### Reuse The Existing LLVM Distribution Contract

`cosmo-jit-sys` should use the same LLVM version, manifest, cache directory, and
toolchain options as `cosmo-clang-sys`. If direct reuse of
`packages/cosmo-clang-sys/CosmoLLVM.cmake` would create an ownership problem,
the implementation should move the helper to a shared CMake location and update
both native packages.

Alternative considered: add a second manifest and download cache for JIT. That
would make the two Clang integrations drift and would double the failure modes
for users configuring `COSMO_LLVM_PATH`, `COSMO_LLVM_OFFLINE`, and
`COSMO_GCC_TOOLCHAIN`.

### Keep Clang Types Behind A C ABI

The public boundary should use `cosmo_jit_sys_*` symbols, string views, opaque
handles, fixed-width scalar values, and explicit dispose functions. Clang,
LLVM, and clang-repl implementation types must stay private to the native
package.

Alternative considered: call clang-repl or Clang C++ APIs directly from
Scala.js or the package driver. That would leak toolchain lifetime and ABI
details into consumers that should only see structured compile-time execution
results.

### Start With A Smoke-Oriented Execution Surface

The first API should be intentionally narrow: create a session, evaluate a C++
snippet, run or materialize a named entry point needed by smoke tests, and
return structured status/diagnostics. The API may be process-backed by the
`clang-repl` executable or in-process through Clang interpreter APIs; consumers
must not depend on that implementation detail.

Alternative considered: design the final macro-provider protocol now. The
macro protocol still depends on reflection, hygiene, and macro output
validation decisions. A smoke-oriented JIT surface proves the risky native
toolchain path without forcing those decisions early.

### Benchmark For Trend Data, Not Absolute Gates

The benchmark should record cold session creation, first snippet evaluation,
warm repeated evaluation, artifact/toolchain identity, host platform, and input
shape. It should include a heavy-header shape that adds the repository
nlohmann JSON include root and evaluates a snippet using `nlohmann::json`.
The report should identify the include root used, for example
`target/cosmo/externals/json/single_include`, so results are attributable to the
same dependency layout as package builds. It should emit structured data under
`target/cosmo/bench/` and keep CI validation to "benchmark runs and reports
measurements" unless a later change adds calibrated thresholds.

Alternative considered: fail CI if startup or evaluation exceeds a fixed
duration. That would be brittle across LLVM builds, local hardware, debug vs
release profiles, and sandboxed CI hosts.

## Risks / Trade-offs

clang-repl APIs or packaging differ across platforms -> Keep the implementation
behind `cosmo-jit-sys`, diagnose missing support clearly, and initially require
the smoke benchmark only on platforms where the manifest provides clang-repl.

Process-backed sessions may be slower than in-process interpreter APIs -> The
benchmark makes the cost visible, and the C ABI leaves room to replace the
backend without changing cosmo0 consumers.

Shared CMake helper movement can disturb `cosmo-clang-sys` -> Keep the helper
refactor mechanical, preserve existing cache option names, and rerun the
current package-run and CMake smoke checks.

## Migration Plan

1. Add or share the CMake/LLVM helper so both native packages resolve the same
   LLVM/Clang install.
2. Add `packages/cosmo-jit-sys` with the `cosmoJit` target and
   `cosmo_jit_sys_*` C ABI.
3. Add smoke execution coverage proving clang-repl runs a C++ snippet and
   returns a deterministic result.
4. Add benchmark coverage for integer, standard-header, and nlohmann JSON
   header inputs, then document the command and output path.
5. Keep package manifests unchanged until a later consumer explicitly requests
   `cosmo-jit-sys`.

## Open Questions

- Whether the first implementation should use the clang-repl executable as a
  subprocess or Clang interpreter APIs directly.
- Whether benchmark comparison baselines should live in versioned fixtures or
  only in generated `target/` artifacts.

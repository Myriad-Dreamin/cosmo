## Context

The earlier compile-time execution direction used clang-repl or
clangInterpreter as the native substrate. That made startup and incremental
execution attractive, but it also made macro semantics depend on an interpreter
mode that can diverge from ordinary Clang compilation and may miscompile code.
Macro provider output must be grounded in the same C++ semantics that the
backend will later compile and link.

This change replaces the interpreter-style JIT direction with a native
compile-time execution component that compiles explicit provider entry
functions. Heavy C++ setup is amortized through PCH, Clang modules, module
caches, or an equivalent precompiled context.

## Goals / Non-Goals

**Goals:**

- Add `packages/cosmo-cte-sys` as the native compile-time execution component.
- Compile provider entry functions as ordinary Clang code.
- Reject clang-repl / clangInterpreter as execution substrates.
- Reuse expensive C++ header and semantic setup through a deterministic
  precompiled context key.
- Expose a stable C ABI that hides Clang and LLVM implementation details.
- Prove the path with integer and standard-library provider-entry smoke tests.
- Benchmark cold context creation, warm context reuse, provider-entry compile,
  load/invoke time, and heavy-header behavior.

**Non-Goals:**

- Implement macro expansion or provider package loading.
- Define general constant evaluation semantics in cosmo0.
- Use clangInterpreter or clang-repl for accepted provider execution.
- Require absolute performance thresholds before the path has baseline data.
- Expose Clang C++ objects across the support-library boundary.

## Decisions

### Compile Provider Entry Functions

The accepted execution unit is a small provider entry function with a stable
exported ABI. The entry receives serialized macro input, calls provider helper
code as needed, and returns serialized macro output.

Alternative considered: evaluate snippets in a clangInterpreter session. That
keeps an interactive workflow, but it makes macro semantics depend on an
interpreter path that can miscompile or diverge from the final Clang-compiled
program.

### Cache A Precompiled Context

Heavy headers and support code should be parsed once per deterministic context
key. The implementation may use PCH, Clang modules, module cache, or another
Clang-owned precompiled representation. The key must include the C++ standard,
target triple, toolchain identity, include paths, headers/imports, compile
options, and support library identities.

Alternative considered: compile each provider entry from a cold translation
unit. That is simpler but makes macro execution latency depend heavily on
repeated header parsing.

### Keep The C ABI Narrow

The component exposes `cosmo_cte_sys_*` symbols, opaque handles, C ABI request
and result structs, diagnostics, artifact summaries, and cache summaries. Clang
and LLVM types stay private.

Alternative considered: let Scala/JS or compiler code call Clang APIs directly.
That leaks toolchain details into consumers and makes backend changes harder.

### Benchmark Completion, Not A Fixed Threshold

The benchmark should record timings and metadata, not fail on uncalibrated
absolute numbers. The first goal is to make cold and warm behavior visible.

Alternative considered: require a fixed speedup over cold compile in CI. That
would be noisy until the implementation and machines are stable.

## Risks / Trade-offs

- PCH invalidation can be subtle -> include all compile-affecting settings in
  the context key and report cache hit/miss/invalidation state.
- Dynamic loading provider entries can be platform-specific -> keep platform
  details behind `cosmo-cte-sys` and start with the platform already supported
  by the native Clang support pipeline.
- Per-function compile may still be expensive -> benchmark heavy headers and
  repeated entry compiles before designing higher-level provider batching.
- Support libraries can leak target runtime assumptions -> keep compile-time
  execution separate from target package execution.

## Migration Plan

1. Replace the clang-repl PoC design with `cosmo-cte-sys`.
2. Add the native component, CMake target, and `cosmo_cte_sys_*` C ABI.
3. Add precompiled context creation/reuse APIs and cache summaries.
4. Add integer and standard-library provider-entry smoke tests.
5. Add benchmark reports under `target/cosmo/bench/cosmo-cte-sys/`.
6. Route later cosmo0 CTE probes through this compile adapter.

## Open Questions

- Whether the first platform should invoke compiled entries through a dynamic
  library, object-file loader, or out-of-process helper executable.
- Whether PCH or Clang modules should be the first concrete precompiled context
  implementation.
- How much provider helper code should be compiled into the precompiled context
  versus each provider entry function.

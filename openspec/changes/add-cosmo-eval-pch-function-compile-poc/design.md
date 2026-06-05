## Context

The earlier compile-time execution direction used clang-repl or
clangInterpreter as the substrate. That made startup and incremental execution
attractive, but it also made macro semantics depend on an interpreter mode that
can diverge from ordinary Clang compilation and may miscompile code. Macro
provider output must be grounded in the same C++ semantics that the backend
will later compile and link.

This change replaces the interpreter-style JIT direction with an eval contract
implemented independently by cosmo0 and cosmoc. Each compiler's eval mode
compiles explicit provider entry functions as ordinary Clang code. Heavy C++
setup is amortized through PCH, Clang modules, module caches, or an equivalent
precompiled context.

## Goals / Non-Goals

**Goals:**

- Add independent eval modes in cosmo0 and cosmoc rather than a separate
  `cosmo-cte-sys` support package.
- Let cosmo0 implement eval sessions, requests, purity inputs, PCH/precompiled
  context caching, provider-entry compilation, and structured results for
  cosmo0 macro and constant-evaluation users.
- Let cosmoc implement the same eval contract independently for cosmoc
  compile-time evaluation and REPL users.
- Compile provider entry functions as ordinary Clang code.
- Reject clang-repl / clangInterpreter as execution substrates.
- Reuse expensive C++ header and semantic setup through a deterministic
  precompiled context key.
- Prove the path with integer and standard-library provider-entry smoke tests.
- Benchmark cold context creation, warm context reuse, provider-entry compile,
  load/invoke time, and heavy-header behavior.

**Non-Goals:**

- Add `packages/cosmo-cte-sys` or a public `cosmo_cte_sys_*` C ABI.
- Implement macro expansion or provider package loading.
- Define general constant evaluation semantics in cosmo0.
- Define the user-facing REPL command syntax; a separate proposal owns that.
- Use clangInterpreter or clang-repl for accepted provider execution.
- Require absolute performance thresholds before the path has baseline data.

## Decisions

### Keep Compiler Eval Implementations Independent

cosmo0 and cosmoc share the eval contract but not the implementation. Each
compiler owns stable request identity, selected input facts, payload
serialization, resource bounds, purity keys, PCH/precompiled context cache
state, provider-entry compilation, artifact invocation, and structured
diagnostics/results for its own users.

Alternative considered: add a standalone native support package. That creates a
second boundary and C ABI before the compilers have stable eval request shapes.
The first implementation should keep orchestration inside each compiler that
already owns its source sessions, package state, and diagnostics.

### Compile Provider Entry Functions

The accepted execution unit is a small provider entry function with a stable
internal ABI. The entry receives serialized macro or eval input, calls provider
helper code as needed, and returns serialized output.

Alternative considered: evaluate snippets in a clangInterpreter session. That
keeps an interactive workflow, but it makes macro semantics depend on an
interpreter path that can miscompile or diverge from the final Clang-compiled
program.

### Cache A Precompiled Context

Heavy headers and support code should be parsed once per deterministic context
key. The implementation may use PCH, Clang modules, module cache, or another
Clang-owned precompiled representation. The key must include the C++ standard,
target triple, toolchain identity, include paths, headers/imports, compile
options, support library identities, and eval-session profile.

Alternative considered: compile each provider entry from a cold translation
unit. That is simpler but makes macro execution and REPL latency depend heavily
on repeated header parsing.

### Keep The Internal Boundary Structured

Each eval module uses `CosmoEvalRequest` and `CosmoEvalResult` records, not raw
compiler mutation handles. Clang and LLVM implementation details stay inside
the compiler's eval implementation. Ordinary compiler phases consume structured
diagnostics, serialized outputs, C++ type facts, artifact summaries, and cache
summaries.

Alternative considered: let high-level compiler phases call Clang APIs
directly. That leaks toolchain details into semantic phases and makes REPL,
macro execution, and constant-evaluation probes harder to share.

### Benchmark Completion, Not A Fixed Threshold

The benchmark should record timings and metadata, not fail on uncalibrated
absolute numbers. The first goal is to make cold and warm behavior visible.

Alternative considered: require a fixed speedup over cold compile in CI. That
would be noisy until the implementation and machines are stable.

## Risks / Trade-offs

- PCH invalidation can be subtle -> include all compile-affecting settings in
  the context key and report cache hit/miss/invalidation state.
- Driver-owned compilation can grow into a large module -> keep request/result,
  cache management, compile, load/invoke, and benchmark code separated inside
  eval.
- Per-function compile may still be expensive -> benchmark heavy headers and
  repeated entry compiles before designing higher-level provider batching.
- Support libraries can leak target runtime assumptions -> keep compile-time
  execution separate from target package execution.

## Migration Plan

1. Replace the clang-repl PoC design with independent cosmo eval modes.
2. Add cosmo0 request/result/session records and eval implementation skeletons.
3. Add cosmoc request/result/session records and eval implementation skeletons.
4. Add precompiled context creation/reuse APIs and cache summaries.
5. Add integer and standard-library provider-entry smoke tests.
6. Add benchmark reports under `target/cosmo/bench/eval/`.
7. Route later cosmo0 CTE probes through cosmo0 eval and cosmoc REPL sessions
   through cosmoc eval.

## Open Questions

- Whether the first platform should invoke compiled entries through a dynamic
  library, object-file loader, or out-of-process helper executable.
- Whether PCH or Clang modules should be the first concrete precompiled context
  implementation.
- How much provider helper code should be compiled into the precompiled context
  versus each provider entry function.

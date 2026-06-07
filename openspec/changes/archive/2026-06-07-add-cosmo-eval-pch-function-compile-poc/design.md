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

The implemented PoC used a PCH-backed executable backend:

- context: `clang++ -std=c++17 ... -x c++-header context.hpp -o context.hpp.pch`
- provider: `clang++ -std=c++17 ... -include-pch context.hpp.pch provider_entry.cpp -o provider_entry`

That backend provides a useful semantic baseline, but measured warm compile
times are still too high for interactive or repeated provider execution. The
change should therefore be read as a contract and benchmark PoC, not as an
accepted production backend for REPL or compile-time evaluation.

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
- Record whether the first backend is only a proof backend or acceptable for
  downstream interactive and compile-time evaluation consumers.

**Non-Goals:**

- Add `packages/cosmo-cte-sys` or a public `cosmo_cte_sys_*` C ABI.
- Implement macro expansion or provider package loading.
- Define general constant evaluation semantics in cosmo0.
- Define the user-facing REPL command syntax; a separate proposal owns that.
- Use clangInterpreter or clang-repl for accepted provider execution.
- Treat the measured PCH executable backend as the final low-latency backend for
  REPL or production compile-time evaluation.
- Design the successor backend in this change; that belongs in a follow-up
  proposal after the benchmark result is reviewed.
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

### Keep PCH Executable As A Proof Backend

The PCH executable backend should remain available as a semantic smoke and
benchmark backend, but downstream REPL and production compile-time evaluation
should not depend on it as the latency-accepted path. It still performs a
provider-entry compile and executable link for each request, and the benchmark
shows that this dominates warm repeated execution.

The current benchmark data:

| Backend | Input | Cold / context | Warm / repeated | Report |
| --- | --- | ---: | ---: | --- |
| Clang 15 PCH executable | integer | 209.40 ms | 67.54 ms | `target/cosmo/bench/eval/eval-2026-06-05T22-23-12-230Z.json` |
| Clang 15 PCH executable | standard | 226.50 ms | 77.22 ms | `target/cosmo/bench/eval/eval-2026-06-05T22-23-12-230Z.json` |
| Clang 15 PCH executable | nlohmann-json | 1029.31 ms | 519.85 ms | `target/cosmo/bench/eval/eval-2026-06-05T22-23-12-230Z.json` |
| Clang 21.1.4 PCH executable | integer | 787.53 ms | 70.71 ms | `target/cosmo/bench/eval/eval-2026-06-06T03-19-28-406Z.json` |
| Clang 21.1.4 PCH executable | standard | 274.67 ms | 84.21 ms | `target/cosmo/bench/eval/eval-2026-06-06T03-19-28-406Z.json` |
| Clang 21.1.4 PCH executable | nlohmann-json | 1173.55 ms | 577.76 ms | `target/cosmo/bench/eval/eval-2026-06-06T03-19-28-406Z.json` |

The benchmark compile options were empty for these inputs. There was no `-O2`,
`-O3`, `-g`, LTO, or similar optimization/debug flag explaining the latency.

For comparison, local Clang 21.1.4 `clang-repl` produced much faster repeated
eval timings, but remains rejected as the accepted semantic substrate:

| Backend | Input | Context | First eval | Repeated eval | Report |
| --- | --- | ---: | ---: | ---: | --- |
| Clang 21.1.4 clang-repl | integer | 135.86 ms | 4.72 ms | 4.66 ms | `target/cosmo/bench/eval/clang-repl-2026-06-06T04-05-01-126Z.json` |
| Clang 21.1.4 clang-repl | standard | 157.42 ms | 20.18 ms | 10.98 ms | `target/cosmo/bench/eval/clang-repl-2026-06-06T04-05-01-126Z.json` |
| Clang 21.1.4 clang-repl | nlohmann-json | 714.78 ms | 330.07 ms | 87.04 ms | `target/cosmo/bench/eval/clang-repl-2026-06-06T04-05-01-126Z.json` |

The successor backend should preserve ordinary Clang compile semantics while
removing the per-request executable compile/link cost. Candidate directions
include a persistent worker that loads compiled objects or shared libraries, an
object-cache plus long-lived loader, or another Clang-owned compiled artifact
path that does not switch provider semantics to clangInterpreter execution.

## Risks / Trade-offs

- PCH invalidation can be subtle -> include all compile-affecting settings in
  the context key and report cache hit/miss/invalidation state.
- Driver-owned compilation can grow into a large module -> keep request/result,
  cache management, compile, load/invoke, and benchmark code separated inside
  eval.
- Per-function compile is expensive in the measured PCH executable backend ->
  treat that backend as a semantic PoC and require a follow-up backend decision
  before REPL or production CTE depends on it.
- Support libraries can leak target runtime assumptions -> keep compile-time
  execution separate from target package execution.

## Migration Plan

1. Replace the clang-repl PoC design with independent cosmo eval modes.
2. Add cosmo0 request/result/session records and eval implementation skeletons.
3. Add cosmoc request/result/session records and eval implementation skeletons.
4. Add precompiled context creation/reuse APIs and cache summaries.
5. Add integer and standard-library provider-entry smoke tests.
6. Add benchmark reports under `target/cosmo/bench/eval/`.
7. Record the benchmark conclusion that PCH executable is a proof backend only.
8. Route later cosmo0 CTE probes and cosmoc REPL sessions through cosmoc/cosmo0
   eval only after a faster ordinary-Clang backend is accepted.

## Open Questions

- Whether the first platform should invoke compiled entries through a dynamic
  library, object-file loader, or out-of-process helper executable.
- Whether the successor should use dynamic libraries, object files, a
  persistent loader process, a Clang module/PCH combination, or another
  Clang-owned compiled artifact path.
- How much provider helper code should be compiled into the precompiled context
  versus each provider entry function.
- What latency budget is acceptable for REPL and production CTE once the
  successor backend is proposed.

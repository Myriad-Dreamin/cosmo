## Why

`docs/cosmo/compile-time-evaluation.typ` rejects the original clang-repl /
clangInterpreter direction because interpreter execution can diverge from
ordinary Clang compilation and may miscompile provider code. The repository
needs an eval contract for cosmo0 and cosmoc that keeps Clang compilation as the
semantic source of truth while measuring whether small provider-entry
compilation can be accelerated with PCH or an equivalent precompiled context.

This eval mode is not a new `cosmo-cte-sys` package. cosmo0 and cosmoc are two
independent compilers, and neither eval implementation may depend on the other.
They share the documented request/result contract, cache semantics, and
provider-entry compile model. cosmo0 implements that contract inside cosmo0 for
macro expansion and constant-evaluation probes. cosmoc implements the same
contract inside cosmoc for compile-time evaluation and REPL sessions.

Benchmarking the implemented PCH executable PoC showed that the semantic model
is useful but the current per-entry executable compile/link backend is too slow
for interactive or repeated provider execution. The PoC therefore establishes
the contract, cache key, smoke coverage, and benchmark evidence. It does not
promote the PCH executable backend as the accepted production backend for REPL
or compile-time evaluation.

## What Changes

- Add a cosmo eval capability covering independent cosmo0 and cosmoc eval
  modes.
- Compile explicit provider entry functions as ordinary Clang translation units
  instead of evaluating snippets through clang-repl or clangInterpreter.
- Add a precompiled context cache using PCH, Clang modules, module cache, or an
  equivalent Clang-owned precompiled representation.
- Reuse the repository LLVM/Clang distribution and discovery conventions
  already used by `cosmo-clang-sys`, without requiring clang-repl artifacts.
- Add internal `CosmoEvalRequest` / `CosmoEvalResult` shapes for compiler
  consumers instead of a public `cosmo_cte_sys_*` C ABI.
- Add a proof-of-concept smoke path that compiles one provider entry function,
  loads or invokes it through the eval module, and returns a structured result.
- Add benchmark coverage for cold context creation, warm PCH/precompiled context
  reuse, first provider-entry compile, repeated provider-entry compile, and a
  heavy-header input that includes `nlohmann/json.hpp`.
- Record the benchmark conclusion that the first PCH executable backend remains
  a proof backend, not a latency-accepted backend for interactive REPL or
  production compile-time evaluation.
- Defer REPL and broader CTE integration to a follow-up backend decision that
  preserves ordinary Clang compile semantics while removing the per-request
  executable compile/link bottleneck.
- Keep macro expansion, full compile-time evaluation integration, and REPL
  command UX outside this change.

## Capabilities

### New Capabilities

- `cosmo-eval`: Defines the common eval contract implemented independently by
  cosmo0 and cosmoc, including internal request and result boundaries,
  PCH/precompiled context cache semantics, single provider-entry compile smoke
  paths, benchmark reporting, and backend maturity status for future
  compile-time evaluation and REPL consumers.

### Modified Capabilities

- None.

## Impact

- Adds or extends eval modules inside the existing cosmo0 and cosmoc compilers;
  it does not add `packages/cosmo-cte-sys`.
- May move or share LLVM/Clang helper code so `cosmo-clang-sys` validation,
  cosmo0 eval, cosmoc eval, and future REPL sessions can use consistent
  manifest, cache, offline mode, and toolchain options without introducing a
  cross-compiler dependency.
- Adds focused eval smoke tests and benchmark scripts or test targets,
  including a benchmark shape for the repository nlohmann JSON header.
- Removes the requirement that the LLVM/Clang distribution include clang-repl.
- Establishes the request/result and benchmark dependency that later
  compile-time evaluation probes and REPL mode can use after a faster
  ordinary-Clang backend is accepted, without adding the full macro system.

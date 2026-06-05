## 1. Eval Module Setup

- [ ] 1.1 Add cosmo0 eval request/result/session model types and implementation
  modules for provider-entry compile execution.
- [ ] 1.2 Add cosmoc eval request/result/session model types and implementation
  modules for provider-entry compile execution.
- [ ] 1.3 Share LLVM/Clang discovery with `cosmo-clang-sys` so eval uses the
  same LLVM version, manifest, cache, offline mode, target platform, target
  architecture, and GNU toolchain options.
- [ ] 1.4 Add configure-time or startup checks for Clang compile,
  PCH/precompiled context, link, and load support.
- [ ] 1.5 Add clear diagnostics for unsupported clang-repl / clangInterpreter
  backend requests.

## 2. Internal Eval Boundary

- [ ] 2.1 Define `CosmoEvalRequest` with provider/eval identity, serialized
  input, C++ imports and headers, include/library search context, generated
  entry source, target settings, resource limits, precompiled context key, and
  toolchain identity.
- [ ] 2.2 Define `CosmoEvalResult` with status, diagnostics, serialized output,
  requested C++ facts, support binding metadata, generated artifact summaries,
  cache summaries, and captured output summaries.
- [ ] 2.3 Ensure the boundary does not expose Clang or LLVM implementation
  objects to ordinary compiler semantic phases.
- [ ] 2.4 Add tests for request/result lifecycle and deterministic serialization.

## 3. Precompiled Context Cache

- [ ] 3.1 Define a deterministic precompiled context key that includes C++
  standard, target triple, toolchain identity, include paths, headers/imports,
  compile options, support library identities, and eval-session profile.
- [ ] 3.2 Implement the first PCH, Clang module, module-cache, or equivalent
  Clang-owned precompiled context path.
- [ ] 3.3 Report cache created/reused/invalidated state in structured results.
- [ ] 3.4 Add tests for same-key reuse and changed-key invalidation.

## 4. Provider Entry Compile Proof

- [ ] 4.1 Add a smoke provider entry function that computes `1 + 1` and returns
  `2` through the structured result.
- [ ] 4.2 Add a smoke provider entry that includes a standard C++ header and
  instantiates a standard library type.
- [ ] 4.3 Compile provider entries as ordinary Clang translation units against
  the precompiled context.
- [ ] 4.4 Load or invoke the compiled entry through the selected eval adapter and
  return structured diagnostics/results.

## 5. Benchmarks

- [ ] 5.1 Add benchmark inputs for cold context creation, warm context reuse,
  first provider-entry compile, repeated provider-entry compile, load/invoke
  time, and total request time.
- [ ] 5.2 Write benchmark reports under `target/cosmo/bench/eval/` with host
  platform, LLVM/Clang version, compile backend identity, build profile, and
  input shape.
- [ ] 5.3 Add a heavy-header benchmark using the repository nlohmann JSON
  include root and a deterministic `nlohmann::json` result.
- [ ] 5.4 Keep benchmark success based on completion and structured reporting,
  not an absolute timing threshold.

## 6. Validation

- [ ] 6.1 Run the eval smoke and benchmark paths on a toolchain with Clang
  compile support available.
- [ ] 6.2 Add failure-path coverage for missing Clang compile/PCH/link/load
  support.
- [ ] 6.3 Run relevant cosmo0 eval tests and cosmoc eval tests.

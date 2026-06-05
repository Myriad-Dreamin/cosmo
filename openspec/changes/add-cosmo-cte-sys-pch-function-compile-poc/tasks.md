## 1. Native Component Setup

- [ ] 1.1 Add `packages/cosmo-cte-sys` with CMake project metadata, source,
  public headers, and a native `cosmoCte` target.
- [ ] 1.2 Share LLVM/Clang discovery with `cosmo-clang-sys` so both components
  use the same LLVM version, manifest, cache, offline mode, target platform,
  target architecture, and GNU toolchain options.
- [ ] 1.3 Add configure-time checks for Clang compile, PCH/precompiled context,
  link, and load support.
- [ ] 1.4 Add a clear diagnostic for unsupported clang-repl / clangInterpreter
  backend requests.

## 2. C ABI Boundary

- [ ] 2.1 Add `include/cosmo_cte_sys.h` with `cosmo_cte_sys_*` status values,
  opaque handles, request structs, result structs, diagnostics, artifact
  summaries, cache summaries, and dispose functions.
- [ ] 2.2 Ensure the C ABI does not expose Clang or LLVM implementation types.
- [ ] 2.3 Add C/C++ tests that create and dispose compile contexts without
  leaking handles or result memory.

## 3. Precompiled Context Cache

- [ ] 3.1 Define a deterministic precompiled context key that includes C++
  standard, target triple, toolchain identity, include paths, headers/imports,
  compile options, and support library identities.
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
- [ ] 4.4 Load or invoke the compiled entry through the selected platform
  adapter and return structured diagnostics/results.

## 5. Benchmarks

- [ ] 5.1 Add benchmark inputs for cold context creation, warm context reuse,
  first provider-entry compile, repeated provider-entry compile, load/invoke
  time, and total request time.
- [ ] 5.2 Write benchmark reports under `target/cosmo/bench/cosmo-cte-sys/`
  with host platform, LLVM/Clang version, compile backend identity, build
  profile, and input shape.
- [ ] 5.3 Add a heavy-header benchmark using the repository nlohmann JSON
  include root and a deterministic `nlohmann::json` result.
- [ ] 5.4 Keep benchmark success based on completion and structured reporting,
  not an absolute timing threshold.

## 6. Validation

- [ ] 6.1 Run the `cosmo-cte-sys` configure, build, smoke, and benchmark paths
  on a toolchain with Clang compile support available.
- [ ] 6.2 Add failure-path coverage for missing Clang compile/PCH/link/load
  support.
- [ ] 6.3 Run relevant native support tests and CMake package tests.

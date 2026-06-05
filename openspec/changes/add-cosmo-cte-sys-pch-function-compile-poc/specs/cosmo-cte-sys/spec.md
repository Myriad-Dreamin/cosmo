## ADDED Requirements

### Requirement: Native Compile-Time Execution Component

The repository SHALL provide a native `cosmo-cte-sys` support component that
uses the repository LLVM/Clang toolchain contract to compile and execute
provider entry functions during compilation.

#### Scenario: cosmo-cte-sys configures with Clang available

- **WHEN** the CMake build configures `cosmo-cte-sys` with an LLVM/Clang
  installation or manifest cache that includes Clang libraries, headers, and a
  supported linker/loader path
- **THEN** it defines a native `cosmoCte` target
- **AND** it produces the platform's native `cosmo-cte-sys` library artifact
- **AND** it uses the same LLVM version, manifest, cache, offline mode, target
  platform, target architecture, and GNU toolchain options as the existing
  Clang support pipeline

#### Scenario: Required Clang compile support is missing

- **WHEN** the CMake build enables `cosmo-cte-sys` but cannot locate the Clang
  compile, PCH/precompiled context, link, or load support required by the
  selected backend
- **THEN** configuration fails with a diagnostic that names the missing support
- **AND** the diagnostic explains how to set `COSMO_LLVM_PATH` or use the
  repository LLVM manifest cache

### Requirement: clangInterpreter Is Not The Execution Substrate

`cosmo-cte-sys` SHALL NOT rely on clang-repl or clangInterpreter as the semantic
execution substrate for provider code.

#### Scenario: Interpreter backend is requested

- **WHEN** a build or runtime option selects clang-repl or clangInterpreter for
  compile-time provider execution
- **THEN** `cosmo-cte-sys` rejects the option with an unsupported-backend
  diagnostic
- **AND** the diagnostic explains that provider entry functions must be compiled
  through the Clang compile path

### Requirement: Stable C ABI Boundary

`cosmo-cte-sys` SHALL expose a stable C ABI for compiler and driver consumers,
and SHALL NOT expose Clang or LLVM implementation types across the library
boundary.

#### Scenario: Consumer creates and disposes a compile context

- **WHEN** a consumer creates a `cosmo-cte-sys` context through the C ABI
- **THEN** exported symbols use the `cosmo_cte_sys_` prefix
- **AND** the consumer receives only opaque handles and C ABI value structs
- **AND** `cosmo-cte-sys` provides matching dispose functions for every owned
  handle or result

#### Scenario: Consumer compiles a provider entry function

- **WHEN** a consumer submits a provider entry function to a compile context
- **THEN** the result reports a stable status code, diagnostics, captured output
  summary, generated artifact summary, and precompiled context cache summary
- **AND** Clang and LLVM implementation objects remain private to
  `cosmo-cte-sys`

### Requirement: PCH Or Precompiled Context Cache

`cosmo-cte-sys` SHALL provide a cache for heavy C++ semantic setup so repeated
provider-entry compilation does not reparse the same headers from scratch.

#### Scenario: Precompiled context is created

- **WHEN** a request declares a C++ standard, target triple, include paths,
  headers/imports, compile options, support libraries, and toolchain identity
- **THEN** `cosmo-cte-sys` computes a deterministic precompiled context key
- **AND** it builds or reuses a PCH, Clang module cache, or equivalent
  Clang-owned precompiled representation for that key

#### Scenario: Provider entry reuses precompiled context

- **WHEN** two provider-entry compile requests have the same precompiled context
  key
- **THEN** the second request reuses the cached context when available
- **AND** the structured result reports whether the context was created, reused,
  or invalidated

### Requirement: Single Provider Entry Compile Proof

`cosmo-cte-sys` SHALL include a proof-of-concept smoke path that compiles and
executes a provider entry function as ordinary Clang-compiled code.

#### Scenario: Integer provider entry executes

- **WHEN** the smoke runner compiles a provider entry function whose exported
  entry point computes `1 + 1`
- **THEN** `cosmo-cte-sys` reports successful compilation and execution
- **AND** the structured result contains the integer value `2`
- **AND** diagnostics are empty or marked non-fatal

#### Scenario: C++ standard library provider entry executes

- **WHEN** the smoke runner compiles a provider entry function that includes a
  standard C++ header and instantiates a standard library type
- **THEN** Clang accepts the entry through `cosmo-cte-sys`
- **AND** the result proves the entry executed under ordinary Clang C++
  compilation semantics

### Requirement: Compile Benchmark Reporting

`cosmo-cte-sys` SHALL provide benchmark coverage that measures the compile-time
execution path and emits structured results suitable for comparison across
commits and toolchains. The benchmark SHALL include both lightweight entries and
a heavy-header input that includes `nlohmann/json.hpp`.

#### Scenario: Benchmark records cold and warm timings

- **WHEN** the `cosmo-cte-sys` benchmark runs
- **THEN** it records cold precompiled context creation time, warm context reuse
  time, first provider-entry compile time, repeated provider-entry compile time,
  load/invoke time, and total request time
- **AND** it records host platform, LLVM/Clang version, compile backend
  identity, build profile, and benchmark input shape
- **AND** it writes a structured report under `target/cosmo/bench/`

#### Scenario: Benchmark includes nlohmann JSON header input

- **WHEN** the `cosmo-cte-sys` benchmark runs with repository external headers
  available
- **THEN** one benchmark input adds the nlohmann JSON include root, such as
  `target/cosmo/externals/json/single_include`
- **AND** the provider entry includes `nlohmann/json.hpp`
- **AND** the entry constructs or parses a `nlohmann::json` value and returns a
  deterministic scalar result
- **AND** the structured report records the input shape as a nlohmann JSON
  heavy-header benchmark

#### Scenario: Benchmark avoids unstable absolute thresholds

- **WHEN** the benchmark is run in CI or on a developer machine
- **THEN** success requires the benchmark to complete and report measurements
- **AND** the benchmark does not fail solely because a machine-specific timing
  exceeds an uncalibrated absolute threshold

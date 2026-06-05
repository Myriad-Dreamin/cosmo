## ADDED Requirements

### Requirement: Independent Eval Modes

The repository SHALL provide independent cosmo0 and cosmoc eval modes that use
the repository LLVM/Clang toolchain contract to compile and execute provider
entry functions during compilation.

#### Scenario: cosmo0 eval configures with Clang available

- **WHEN** cosmo0 starts an eval session with an LLVM/Clang installation or
  manifest cache that includes Clang libraries, headers, and a supported
  linker/loader path
- **THEN** cosmo0 eval records the selected toolchain identity
- **AND** it uses the same LLVM version, manifest, cache, offline mode, target
  platform, target architecture, and GNU toolchain options as the existing
  Clang support pipeline
- **AND** no separate `cosmo-cte-sys` native package is required

#### Scenario: cosmoc eval configures with Clang available

- **WHEN** cosmoc starts an eval session with an LLVM/Clang installation or
  manifest cache that includes Clang libraries, headers, and a supported
  linker/loader path
- **THEN** cosmoc eval records the selected toolchain identity
- **AND** it uses the same LLVM version, manifest, cache, offline mode, target
  platform, target architecture, and GNU toolchain options as the existing
  Clang support pipeline
- **AND** it does not call into cosmo0 eval for execution

#### Scenario: Required Clang compile support is missing

- **WHEN** eval mode is enabled but cosmoc cannot locate the Clang compile,
  PCH/precompiled context, link, or load support required by the selected
  backend
- **THEN** it reports a diagnostic that names the missing support
- **AND** the diagnostic explains how to set `COSMO_LLVM_PATH` or use the
  repository LLVM manifest cache

### Requirement: clangInterpreter Is Not The Execution Substrate

Eval mode SHALL NOT rely on clang-repl or clangInterpreter as the semantic
execution substrate for provider code.

#### Scenario: Interpreter backend is requested

- **WHEN** a build, test, or REPL option selects clang-repl or clangInterpreter
  for compile-time provider execution
- **THEN** eval mode rejects the option with an unsupported-backend diagnostic
- **AND** the diagnostic explains that provider entry functions must be compiled
  through the ordinary Clang compile path

### Requirement: Internal Eval Request Boundary

Eval mode SHALL expose an internal `CosmoEvalRequest` / `CosmoEvalResult`
boundary inside each compiler, and SHALL NOT expose Clang or LLVM
implementation types across that boundary.

#### Scenario: cosmo0 submits an eval request to cosmo0 eval

- **WHEN** cosmo0 needs C++ compile-time execution for a macro, probe, or REPL
  input
- **THEN** it submits a structured request with eval identity, serialized input,
  C++ imports and headers, generated provider entry source, target settings,
  bounds, precompiled context key, compile options, and toolchain identity
- **AND** cosmo0 eval returns a structured result with status, diagnostics,
  serialized output, requested C++ facts, support binding metadata, generated
  artifact summaries, and cache summaries
- **AND** the request does not call into cosmoc

#### Scenario: cosmoc submits an eval request to cosmoc eval

- **WHEN** cosmoc needs C++ compile-time execution for compiler features or REPL
  input
- **THEN** it submits a structured request with eval identity, serialized input,
  C++ imports and headers, generated provider entry source, target settings,
  bounds, precompiled context key, compile options, and toolchain identity
- **AND** cosmoc eval returns a structured result with status, diagnostics,
  serialized output, requested C++ facts, support binding metadata, generated
  artifact summaries, and cache summaries
- **AND** the request does not call into cosmo0 eval

#### Scenario: Clang objects remain driver-owned

- **WHEN** an eval implementation compiles or loads provider entry artifacts
- **THEN** Clang and LLVM implementation objects remain private to that eval
  implementation
- **AND** ordinary compiler phases receive only structured values and
  diagnostics

### Requirement: PCH Or Precompiled Context Cache

Eval mode SHALL provide a cache for heavy C++ semantic setup so repeated
provider-entry compilation does not reparse the same headers from scratch.

#### Scenario: Precompiled context is created

- **WHEN** a request declares a C++ standard, target triple, include paths,
  headers/imports, compile options, support libraries, eval-session profile, and
  toolchain identity
- **THEN** eval mode computes a deterministic precompiled context key
- **AND** it builds or reuses a PCH, Clang module cache, or equivalent
  Clang-owned precompiled representation for that key

#### Scenario: Provider entry reuses precompiled context

- **WHEN** two provider-entry compile requests have the same precompiled context
  key
- **THEN** the second request reuses the cached context when available
- **AND** the structured result reports whether the context was created, reused,
  or invalidated

### Requirement: Single Provider Entry Compile Proof

Eval mode SHALL include a proof-of-concept smoke path that compiles and executes
a provider entry function as ordinary Clang-compiled code.

#### Scenario: Integer provider entry executes

- **WHEN** the smoke runner compiles a provider entry function whose exported
  entry point computes `1 + 1`
- **THEN** eval mode reports successful compilation and execution
- **AND** the structured result contains the integer value `2`
- **AND** diagnostics are empty or marked non-fatal

#### Scenario: C++ standard library provider entry executes

- **WHEN** the smoke runner compiles a provider entry function that includes a
  standard C++ header and instantiates a standard library type
- **THEN** Clang accepts the entry through eval mode
- **AND** the result proves the entry executed under ordinary Clang C++
  compilation semantics

### Requirement: Compile Benchmark Reporting

Eval mode SHALL provide benchmark coverage that measures the compile-time
execution path and emits structured results suitable for comparison across
commits and toolchains. The benchmark SHALL include both lightweight entries and
a heavy-header input that includes `nlohmann/json.hpp`.

#### Scenario: Benchmark records cold and warm timings

- **WHEN** the eval benchmark runs
- **THEN** it records cold precompiled context creation time, warm context reuse
  time, first provider-entry compile time, repeated provider-entry compile time,
  load/invoke time, and total request time
- **AND** it records host platform, LLVM/Clang version, compile backend
  identity, build profile, and benchmark input shape
- **AND** it writes a structured report under `target/cosmo/bench/eval/`

#### Scenario: Benchmark includes nlohmann JSON header input

- **WHEN** the eval benchmark runs with repository external headers available
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

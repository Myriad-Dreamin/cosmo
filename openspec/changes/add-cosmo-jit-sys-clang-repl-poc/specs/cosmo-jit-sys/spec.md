## ADDED Requirements

### Requirement: Native clang-repl Support Component

The repository SHALL provide a native `cosmo-jit-sys` support component that
uses the repository LLVM/Clang toolchain contract to run clang-repl based C++
compile-time execution.

#### Scenario: cosmo-jit-sys configures with clang-repl available

- **WHEN** the CMake build configures `cosmo-jit-sys` with an LLVM/Clang
  installation or manifest cache that includes clang-repl support
- **THEN** it defines a native `cosmoJit` target
- **AND** it produces the platform's native `cosmo-jit-sys` library artifact
- **AND** it uses the same LLVM version, manifest, cache, offline mode, target
  platform, target architecture, and GNU toolchain options as the existing
  Clang support pipeline

#### Scenario: clang-repl support is missing

- **WHEN** the CMake build enables `cosmo-jit-sys` but cannot locate the
  required clang-repl executable, library, or headers for the selected backend
- **THEN** configuration fails with a diagnostic that names the missing
  clang-repl requirement
- **AND** the diagnostic explains how to set `COSMO_LLVM_PATH` or use the
  repository LLVM manifest cache

### Requirement: Stable C ABI Boundary

`cosmo-jit-sys` SHALL expose a stable C ABI for compiler and driver consumers,
and SHALL NOT expose Clang, LLVM, or clang-repl implementation types across the
library boundary.

#### Scenario: Consumer creates and disposes a JIT session

- **WHEN** a consumer creates a `cosmo-jit-sys` session through the C ABI
- **THEN** exported symbols use the `cosmo_jit_sys_` prefix
- **AND** the consumer receives only opaque handles and C ABI value structs
- **AND** `cosmo-jit-sys` provides matching dispose functions for every owned
  handle or result

#### Scenario: Consumer evaluates a snippet

- **WHEN** a consumer submits a C++ snippet to an active `cosmo-jit-sys` session
- **THEN** the result reports a stable status code, diagnostics, captured output
  summary, and generated artifact summary when applicable
- **AND** Clang and LLVM implementation objects remain private to
  `cosmo-jit-sys`

### Requirement: clang-repl Execution Proof

`cosmo-jit-sys` SHALL include a proof-of-concept smoke path that demonstrates
clang-repl can execute C++ code, not merely that the toolchain can be located.

#### Scenario: Integer smoke snippet executes

- **WHEN** the smoke runner evaluates a C++ snippet whose exported entry point
  computes `1 + 1`
- **THEN** `cosmo-jit-sys` reports successful execution
- **AND** the structured result contains the integer value `2`
- **AND** diagnostics are empty or marked non-fatal

#### Scenario: C++ standard library smoke snippet executes

- **WHEN** the smoke runner evaluates a C++ snippet that includes a standard C++
  header and instantiates a standard library type
- **THEN** clang-repl accepts the snippet through `cosmo-jit-sys`
- **AND** the result proves the snippet executed under Clang's C++ semantics

### Requirement: JIT Benchmark Reporting

`cosmo-jit-sys` SHALL provide benchmark coverage that measures the JIT path
used by the proof of concept and emits structured results suitable for
comparison across commits and toolchains. The benchmark SHALL include both
lightweight snippets and a heavy-header input that includes `nlohmann/json.hpp`.

#### Scenario: Benchmark records cold and warm timings

- **WHEN** the `cosmo-jit-sys` benchmark runs
- **THEN** it records cold session startup time, first snippet evaluation time,
  and warm repeated evaluation time
- **AND** it records host platform, LLVM/Clang version, clang-repl backend
  identity, build profile, and benchmark input shape
- **AND** it writes a structured report under `target/cosmo/bench/`

#### Scenario: Benchmark includes nlohmann JSON header input

- **WHEN** the `cosmo-jit-sys` benchmark runs with repository external headers
  available
- **THEN** one benchmark input adds the nlohmann JSON include root, such as
  `target/cosmo/externals/json/single_include`
- **AND** the evaluated C++ snippet includes `nlohmann/json.hpp`
- **AND** the snippet constructs or parses a `nlohmann::json` value and returns
  a deterministic scalar result
- **AND** the structured report records the input shape as a nlohmann JSON
  heavy-header benchmark

#### Scenario: Benchmark avoids unstable absolute thresholds

- **WHEN** the benchmark is run in CI or a developer machine
- **THEN** success requires the benchmark to complete and report measurements
- **AND** the benchmark does not fail solely because a machine-specific timing
  exceeds an uncalibrated absolute threshold

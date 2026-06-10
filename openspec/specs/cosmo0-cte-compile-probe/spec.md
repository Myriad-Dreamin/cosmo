# cosmo0-cte-compile-probe Specification

## Purpose
TBD - created by archiving change probe-cosmo0-cte-with-cosmo-eval. Update Purpose after archive.
## Requirements
### Requirement: Gated Compile-Time Evaluation Probe

cosmo0 SHALL provide an experimental compile-time evaluation probe that is
disabled by default and enabled only for focused tests or explicit developer
opt-in.

#### Scenario: Probe is disabled by default

- **WHEN** a normal cosmo0 compile or check run encounters `type x = 1 + 1`
  without the probe enabled
- **THEN** cosmo0 preserves the existing unsupported type-alias target behavior
- **AND** it does not route the expression through cosmo0 eval mode

#### Scenario: Probe is enabled explicitly

- **WHEN** the compile-time evaluation probe is enabled for a focused test or
  developer run
- **AND** the source contains the smoke declaration `type x = 1 + 1`
- **THEN** cosmo0 identifies the declaration as a probe-supported compile-time
  evaluation request
- **AND** unsupported compile-time expression shapes remain outside the probe

### Requirement: Cosmo Eval Request Boundary

The probe SHALL route the smoke expression through cosmo0 eval mode using a
structured request/result boundary rather than evaluating the arithmetic in a
Scala, JavaScript, clangInterpreter, or handwritten host interpreter.

#### Scenario: Smoke expression is evaluated by compiled provider entry

- **WHEN** the enabled probe evaluates `type x = 1 + 1`
- **THEN** cosmo0 sends a `CosmoEvalRequest` containing the source identity,
  declaration identity, expression payload, target settings, precompiled context
  key, and toolchain identity needed by the smoke
- **AND** cosmo0 eval compiles a small provider entry function through Clang
- **AND** cosmo0 consumes the structured `CosmoEvalResult`

#### Scenario: Interpreter or host approximation is not used

- **WHEN** the probe computes the smoke result
- **THEN** it does not use clang-repl, clangInterpreter, a JavaScript arithmetic
  evaluator, a Scala-only constant folder, or a handwritten C++ semantic
  approximation for the accepted probe path

### Requirement: Smoke Alias Result

The probe SHALL make the smoke declaration observable as the computed
compile-time value `2` in the probe result.

#### Scenario: type x equals two

- **WHEN** the enabled probe checks a module containing `type x = 1 + 1`
- **AND** eval mode returns a successful integer result
- **THEN** the probe records that `x` evaluated to `2`
- **AND** the check reports no compile-time evaluation diagnostic for that
  declaration

#### Scenario: Non-smoke declarations are not accepted

- **WHEN** the enabled probe encounters a compile-time expression other than
  the supported integer addition smoke shape
- **THEN** cosmo0 reports that the expression is outside the temporary
  compile-time evaluation probe
- **AND** it does not treat the expression as accepted full compile-time
  evaluation

### Requirement: Compile Execution Failure Diagnostics

The probe SHALL report stable diagnostics when eval mode is disabled,
unavailable, cannot compile the provider entry, or returns a failed execution
result.

#### Scenario: Eval dependency unavailable

- **WHEN** the probe is enabled but eval mode cannot be configured or accessed
- **THEN** cosmo0 reports a diagnostic with code
  `cosmo0.cte-compile-probe.unavailable`
- **AND** the diagnostic includes the source span of the compile-time
  declaration being evaluated

#### Scenario: Provider entry compile fails

- **WHEN** eval mode returns a failed compile status for the smoke request
- **THEN** cosmo0 reports a diagnostic with code
  `cosmo0.cte-compile-probe.compile-failed`
- **AND** it includes the structured Clang diagnostic summary without exposing
  Clang or LLVM implementation objects

#### Scenario: Provider entry execution fails

- **WHEN** eval mode compiles the provider entry but execution fails
- **THEN** cosmo0 reports a diagnostic with code
  `cosmo0.cte-compile-probe.execution-failed`
- **AND** it includes the structured execution diagnostic summary


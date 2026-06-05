## 1. Preconditions

- [ ] 1.1 Complete or depend on `add-cosmo-eval-pch-function-compile-poc` so a
  cosmo0 eval smoke path is available.
- [ ] 1.2 Decide whether the probe runs from a Scala.js compiler adapter, a Node
  driver harness, or a native integration test while native FFI boundaries are
  still limited.

## 2. Probe Boundary

- [ ] 2.1 Add a minimal `CosmoEvalRequest`/`CosmoEvalResult` adapter for the
  probe that carries source identity, declaration identity, expression payload,
  provider entry source, precompiled context key, target settings, compile
  options, toolchain identity, diagnostics, and integer smoke result.
- [ ] 2.2 Add an explicit feature flag, test option, or harness-only entry point
  that keeps the compile-time evaluation probe disabled by default.
- [ ] 2.3 Route the enabled probe through cosmo0 eval mode rather than a
  JavaScript, Scala, clangInterpreter, clang-repl, or handwritten arithmetic
  evaluator.

## 3. Smoke Recognition

- [ ] 3.1 Recognize only the top-level smoke declaration `type x = 1 + 1` when
  the probe is enabled.
- [ ] 3.2 Convert the recognized smoke expression into the provider entry source
  or payload expected by `CosmoEvalRequest`.
- [ ] 3.3 Record successful result `2` as a probe result for alias `x` without
  treating arbitrary type-level arithmetic as accepted source behavior.

## 4. Diagnostics

- [ ] 4.1 Preserve the existing unsupported type-alias behavior when the probe
  is disabled.
- [ ] 4.2 Add `cosmo0.cte-compile-probe.unavailable` when the enabled probe
  cannot configure or access eval mode.
- [ ] 4.3 Add `cosmo0.cte-compile-probe.compile-failed` when eval mode returns a
  failed provider-entry compile result.
- [ ] 4.4 Add `cosmo0.cte-compile-probe.execution-failed` when the compiled
  provider entry cannot be invoked or returns a failed execution result.
- [ ] 4.5 Add a diagnostic for expressions outside the temporary probe shape.

## 5. Validation

- [ ] 5.1 Add a focused test or fixture proving enabled `type x = 1 + 1`
  produces probe result `2`.
- [ ] 5.2 Add disabled-probe coverage proving normal cosmo0 behavior is
  unchanged.
- [ ] 5.3 Add failure-path coverage for unavailable eval support, failed
  provider entry compile, and failed provider entry execution.
- [ ] 5.4 Run the relevant cosmo0 tests and `scripts/check-scala-style.sh` if
  Scala sources are edited.

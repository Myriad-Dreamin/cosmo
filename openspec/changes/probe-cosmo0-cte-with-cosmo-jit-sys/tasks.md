## 1. Preconditions

- [ ] 1.1 Complete or depend on `add-cosmo-jit-sys-clang-repl-poc` so a
  `cosmo-jit-sys` smoke API is available.
- [ ] 1.2 Decide whether the probe runs from a Scala.js compiler adapter, a Node
  driver harness, or a native integration test while native FFI boundaries are
  still limited.

## 2. Probe Boundary

- [ ] 2.1 Add a minimal `CosmoJitRequest`/`CosmoJitResult` adapter for the probe
  that carries source identity, declaration identity, expression payload, target
  settings, toolchain identity, diagnostics, and integer smoke result.
- [ ] 2.2 Add an explicit feature flag, test option, or harness-only entry point
  that keeps the compile-time evaluation probe disabled by default.
- [ ] 2.3 Route the enabled probe through `cosmo-jit-sys` rather than a
  JavaScript, Scala, or handwritten arithmetic evaluator.

## 3. Smoke Recognition

- [ ] 3.1 Recognize only the top-level smoke declaration `type x = 1 + 1` when
  the probe is enabled.
- [ ] 3.2 Convert the recognized smoke expression into the C++ snippet or
  payload expected by `cosmo-jit-sys`.
- [ ] 3.3 Record successful result `2` as a probe result for alias `x` without
  treating arbitrary type-level arithmetic as accepted source behavior.

## 4. Diagnostics

- [ ] 4.1 Preserve the existing unsupported type-alias behavior when the probe
  is disabled.
- [ ] 4.2 Add `cosmo0.cte-jit-probe.unavailable` when the enabled probe cannot
  load or configure `cosmo-jit-sys`.
- [ ] 4.3 Add `cosmo0.cte-jit-probe.execution-failed` when `cosmo-jit-sys`
  returns a failed execution result.
- [ ] 4.4 Add a diagnostic for expressions outside the temporary probe shape.

## 5. Validation

- [ ] 5.1 Add a focused test or fixture proving enabled `type x = 1 + 1`
  produces probe result `2`.
- [ ] 5.2 Add disabled-probe coverage proving normal cosmo0 behavior is
  unchanged.
- [ ] 5.3 Add failure-path coverage for unavailable JIT and failed JIT result.
- [ ] 5.4 Run the relevant cosmo0 tests and `scripts/check-scala-style.sh` if
  Scala sources are edited.

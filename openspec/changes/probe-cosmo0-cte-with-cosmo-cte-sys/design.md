## Context

`docs/cosmo/compile-time-evaluation.typ` defines the long-term compile-time
evaluation model around macro function input/output records and C++ execution
through `cosmo-cte-sys`. The active macro-system proposal owns that full model.
This change is narrower: after `cosmo-cte-sys` exists, prove that cosmo0 can
make a structured compile-time execution request during compilation and consume
a structured result.

The chosen smoke input is intentionally small: `type x = 1 + 1`. It exercises
the "compile-time value affects type-level declaration" path without requiring
the full macro protocol.

## Goals / Non-Goals

**Goals:**

- Add a gated compile-time evaluation probe in cosmo0.
- Lower the smoke expression `1 + 1` into a `cosmo-cte-sys` request.
- Compile a small provider entry function through Clang, optionally using a
  precompiled context.
- Verify that successful execution makes `type x = 1 + 1` resolve to `2` in
  the probe result.
- Return stable diagnostics when the CTE dependency is disabled, unavailable,
  cannot compile the provider entry, or returns a failing execution result.

**Non-Goals:**

- Accept arbitrary type-level arithmetic in normal cosmo0 programs.
- Implement macro providers, macro function records, reflection metadata,
  hygiene, generated declarations, or generated expression validation.
- Replace the existing type checker with a compile-time execution evaluator.
- Use clang-repl or clangInterpreter for the accepted probe path.
- Require the target package executable to be built before compile-time
  evaluation.

## Decisions

### Gate The Probe Explicitly

The probe should be enabled only through a test harness, feature flag, or
driver option. Without that opt-in, normal cosmo0 behavior remains unchanged and
unsupported compile-time expression forms continue to produce existing
diagnostics.

Alternative considered: immediately make `type x = 1 + 1` accepted source.
That would silently add source-facing type-level arithmetic before the full type
and macro semantics are ready.

### Use A Structured Request Instead Of Shelling Out From The Typer

The cosmo0 side should call a small adapter that models a `CosmoCteRequest` and
`CosmoCteResult` shape, even if the first implementation is test-only. The
adapter should pass provider identity, source identity, provider entry source,
precompiled context key, target settings, compile options, and toolchain
identity through `cosmo-cte-sys`.

Alternative considered: invoke Clang directly from a Scala test. That would
prove less than the desired compiler boundary because it bypasses
`cosmo-cte-sys` and would not exercise the future macro execution substrate.

### Restrict The Recognized Expression Shape

The implementation should recognize only the literal smoke shape needed for the
test: one integer addition in a type alias initializer. Anything else remains
unsupported by this probe and is left to the macro-system change.

Alternative considered: add a small interpreter for integer expressions. That
would conflict with the documentation rule that compile-time C++ capability
uses the native execution adapter, and it would distract from proving the
boundary.

## Risks / Trade-offs

Probe code may become accidental production semantics -> Keep it gated, name it
as a probe in APIs and diagnostics, and add non-goal comments around the
recognizer.

Native compile-time execution availability can make tests platform-sensitive ->
Split pure adapter tests from toolchain-backed integration tests and skip or
diagnose the native smoke when `cosmo-cte-sys` is unavailable.

The smoke input looks like a general language feature -> Documentation and
diagnostics must state that this change does not admit general type-level
arithmetic.

## Migration Plan

1. Land `add-cosmo-cte-sys-pch-function-compile-poc`.
2. Add a cosmo0 CTE adapter that consumes the minimal `cosmo-cte-sys` C ABI or
   a test double with the same request/result shape.
3. Add the gated recognizer for `type x = 1 + 1`.
4. Add tests for successful result `2`, disabled/unavailable CTE diagnostics,
   failed provider-entry compile, and failed provider-entry execution.
5. Leave the probe isolated so `introduce-cosmo0-macro-system` can replace or
   absorb it when implementing the full compile-time evaluation boundary.

## Open Questions

- Whether the probe should live in the Scala.js compiler package, a Node driver
  harness, or a native-only integration test until compiler native FFI is
  settled.
- Whether the smoke result should be represented as an integer type-level value
  placeholder or as a test-only alias evaluation record.

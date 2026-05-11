## Context

Diagnostics need to be rendered or recorded by Stage 1. The output mechanism should be small and stable. Diagnostic formatting should not live in std; std only provides the sink.

## Goals / Non-Goals

**Goals:**

- Define minimal standard text output APIs.
- Add runtime binding support behind std APIs.
- Add cosmo1 diagnostic rendering that uses the output capability.
- Add deterministic output tests.

**Non-Goals:**

- Add rich formatting traits.
- Add filesystem writing unless already provided by `core0.path-fs`.
- Add command execution.

## Decisions

### Sink, Not Formatter

The standard API provides text sinks. `driver/diagnostic.cos` owns how diagnostics are formatted.

### Determinism Is Required

Output order and text should be stable in tests because diagnostics are compiler-facing output.

### Runtime Binding Is Hidden

`Stdout`, `Stderr`, or equivalent functions may be extern-backed, but source-level use goes through std APIs.

## Risks / Trade-offs

- Risk: output APIs overlap with string builder APIs.
  Mitigation: keep builder/text construction in `core0.text` and sink behavior in `core0.text-output`.

- Risk: diagnostics start depending on host-specific output behavior.
  Mitigation: tests should use deterministic captured output where possible.

## Migration Plan

1. Update `docs/cosmo0/std.typ` and `runtime.typ`.
2. Add minimal output std APIs.
3. Add backend or extern binding support.
4. Add cosmo1 diagnostic rendering and tests.

## Open Questions

- Should Stage 1 expose concrete `Stdout`/`Stderr` modules or an abstract `TextWriter`?
- Should output functions return `Unit` or a fallible `Result<Unit, IoError>`?

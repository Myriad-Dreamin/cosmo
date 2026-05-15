## Context

`parser.cos` uses nested `if`/`else`, `while`, early `return`, and blocks with local variables. This proposal validates complete parser-source function bodies after basic and member expression typing exist.

## Goals / Non-Goals

**Goals:**

- Type-check conditionals and loops.
- Validate function return compatibility for parser methods and helpers.
- Produce complete typed output for `parser.cos`.

**Non-Goals:**

- Exhaustiveness checking.
- Dataflow optimization.
- Exception or effect typing.

## Decisions

- Require `Bool` conditions for `if` and `while`.
- Treat `while` expressions as `Unit` for parser self-compile.
- Track early returns enough to avoid false missing-return diagnostics.

## Risks / Trade-offs

- Risk: return analysis becomes complex. Mitigation: implement conservative parser-source path checks and keep full control-flow analysis for later.

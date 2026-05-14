## Context

After basic and member lowering, control flow is the remaining barrier to full parser-source IR. The output should be deterministic and pass the verifier before C++ emission begins.

## Goals / Non-Goals

**Goals:**

- Lower conditionals, loops, returns, and short-circuit boolean operators.
- Produce complete verified IR for `parser.cos`.
- Keep labels stable for tests.

**Non-Goals:**

- CFG optimization.
- Pattern matching.
- Exception or effect lowering.

## Decisions

- Use explicit condition, body, else, merge, and exit blocks.
- Generate labels with deterministic counters.
- Treat already-terminated blocks conservatively.

## Risks / Trade-offs

- Risk: nested return lowering creates unreachable blocks. Mitigation: verifier allows well-formed unreachable blocks only when they have explicit terminators.

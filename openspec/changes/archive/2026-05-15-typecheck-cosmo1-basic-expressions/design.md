## Context

`parser.cos` contains many simple statements and expressions. Checking them before method calls and control flow gives the type checker a stable core.

## Goals / Non-Goals

**Goals:**

- Type-check local bindings and simple expressions.
- Enforce assignment and return type compatibility.
- Produce typed expression output.

**Non-Goals:**

- Field access and methods.
- `if`/`while` flow typing.
- Short-circuit lowering.

## Decisions

- Treat blocks as scoped sequences with a final expression type.
- Require `var` for assignment targets where mutation is needed.
- Keep arithmetic and comparison limited to primitive parser-source types.

## Risks / Trade-offs

- Risk: return checking without control flow is incomplete. Mitigation: this slice checks explicit return expressions and leaves path completeness to the control-flow proposal.

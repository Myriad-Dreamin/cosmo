## Context

IR lowering should first prove that the typed module shape can be represented independently from body lowering. This mirrors compiler structure and makes symbol stability testable early.

## Goals / Non-Goals

**Goals:**

- Lower classes, fields, functions, methods, params, and receiver metadata.
- Produce stable IR declaration ordering and names.
- Keep body lowering separate.

**Non-Goals:**

- Expression lowering.
- C++ emission.
- Optimization.

## Decisions

- Use typed declaration ids as inputs to stable IR symbol generation.
- Emit placeholder bodies only where needed by the verifier.
- Preserve method ownership explicitly in IR.

## Risks / Trade-offs

- Risk: placeholder bodies can mask later body-lowering issues. Mitigation: tests in this proposal assert declaration shape only and later proposals replace placeholders.

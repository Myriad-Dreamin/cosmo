## Context

Lowering directly to C++ would hide semantic errors in emission. A small verified IR gives cosmo1 a stable boundary between typed syntax and generated code.

## Goals / Non-Goals

**Goals:**

- Define IR data for parser-source compilation.
- Verify block, local, call, and return consistency.
- Render deterministic IR snapshots.

**Non-Goals:**

- Optimization.
- SSA completeness.
- Full backend ABI design.

## Decisions

- Use explicit blocks and terminators.
- Represent locals and temporaries with stable ids.
- Verify IR before any C++ emission.

## Risks / Trade-offs

- Risk: IR shape changes while lowering grows. Mitigation: keep verifier and renderer close to parser-source requirements and evolve by explicit later proposals.

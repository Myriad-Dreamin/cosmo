## Context

The parser-source IR path needs stable local and temporary handling. Lowering simple expressions first lets later proposals add members and branches without reworking local allocation.

## Goals / Non-Goals

**Goals:**

- Lower basic typed expressions to verifier-accepted IR.
- Allocate locals and temporaries deterministically.
- Preserve return and assignment semantics.

**Non-Goals:**

- Field and method lowering.
- Branch and loop lowering.
- C++ emission.

## Decisions

- Use explicit temporary locals for intermediate expression results.
- Lower direct calls only when the callee is already resolved.
- Require every produced function body to pass the IR verifier.

## Risks / Trade-offs

- Risk: temporary-heavy IR is verbose. Mitigation: favor clarity and verifier simplicity before optimization.

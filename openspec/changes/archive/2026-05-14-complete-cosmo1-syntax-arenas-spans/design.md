## Context

The current syntax arena experiment is too small for parser self-compile. Later compiler stages need stable node ids, repeatable traversal, and spans for diagnostics.

## Goals / Non-Goals

**Goals:**

- Store syntax nodes in cosmo1-owned arenas.
- Preserve spans on nodes that can produce diagnostics.
- Provide deterministic debug output for fixture tests.

**Non-Goals:**

- Final full-language AST shape.
- Serialization format stability.
- Incremental parsing support.

## Decisions

- Use arena ids for syntax references instead of raw pointers.
- Keep expression, type-expression, declaration, and member id spaces explicit.
- Require spans on every accepted parser-source node.

## Risks / Trade-offs

- Risk: additional node ids add boilerplate early. Mitigation: keep helper constructors and debug rendering small and test-focused.

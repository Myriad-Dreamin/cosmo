## Context

The current parser library can determine whether a source is accepted, but downstream cosmo1 stages need syntax nodes. The first parser milestone should therefore produce a minimal but structured tree for the exact constructs exercised by `parser.cos`.

## Goals / Non-Goals

**Goals:**

- Produce syntax data for the `parser.cos` subset.
- Preserve a simple parser surface that can still be compiled through cosmo0.
- Emit enough structure for declaration collection and type-expression resolution.

**Non-Goals:**

- Full Cosmo parser parity.
- Rich error recovery or IDE-quality diagnostics.
- JSON AST bridge support.

## Decisions

- Start with parser-source acceptance constructs instead of all existing Scala parser nodes.
- Represent unsupported constructs explicitly as errors or missing nodes rather than silently accepting them.
- Keep tokenization and parsing boundaries simple enough for cosmo0 compilation.

## Risks / Trade-offs

- Risk: parser output shape changes during later syntax arena work. Mitigation: keep this proposal focused on node kinds and acceptance fixtures, not final storage layout.

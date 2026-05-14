## Context

The type checker needs a complete signature table before expression checking. `parser.cos` uses classes with fields and methods, top-level functions, aliases, explicit parameter annotations, and `&self`/`&mut self`.

## Goals / Non-Goals

**Goals:**

- Build typed declaration signatures.
- Resolve type annotations and aliases.
- Validate method receiver shape.

**Non-Goals:**

- Function body type checking.
- Generic substitution.
- Trait method lookup.

## Decisions

- Resolve declaration signatures before any expression body.
- Keep class field and method signatures available by class definition id.
- Treat invalid receiver types as declaration diagnostics.

## Risks / Trade-offs

- Risk: aliases can create cycles. Mitigation: include alias cycle diagnostics in this declaration-resolution slice.

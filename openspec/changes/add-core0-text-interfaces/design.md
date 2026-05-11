## Context

Lexing and diagnostics need text length, indexing, slicing, and incremental string construction. Existing descriptor-backed string operations are useful implementation scaffolding, but the public API should be owned by core0/std.

## Goals / Non-Goals

**Goals:**

- Define the Stage 1 text API surface in `docs/cosmo0/std.typ`.
- Support source text storage, slicing, char/byte access, and builder-like construction.
- Add cosmo1 source/source-map helpers that use the API.
- Add tests through both direct cosmo0 usage and cosmo1 source.

**Non-Goals:**

- Define full Unicode semantics.
- Implement formatting traits.
- Add filesystem loading; that belongs to `add-core0-path-fs`.

## Decisions

### Stage 1 Text Semantics Are Explicitly Minimal

Stage 1 should define the indexing and classification semantics needed by the lexer. If only ASCII-oriented lexing is supported initially, the spec must say so.

### Public API Lives In Std

Temporary backend or descriptor implementation support may exist, but source-level availability is checked through `core0.text`.

### Builder Is A Text Facility

Builder-like construction belongs to `core0.text` or a closely related text capability. It is not a separate descriptor family.

## Risks / Trade-offs

- Risk: early text API overcommits to Unicode behavior.
  Mitigation: specify only what Stage 1 needs and leave broader text semantics for later.

- Risk: implementation still reuses existing descriptor lowering.
  Mitigation: tests and docs assert std API ownership even if implementation remains transitional.

## Migration Plan

1. Update `docs/cosmo0/type.typ`, `std.typ`, and `runtime.typ`.
2. Add text interface declarations or source modules.
3. Add implementation or extern/backend binding.
4. Add cosmo1 source/source-map helpers and tests.

## Open Questions

- Should the first text view type be a separate `TextSlice` or represented by `String` plus offsets?
- Should builder-like APIs live under `core0.text` or `core0.text-output`?

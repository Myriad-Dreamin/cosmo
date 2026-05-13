## Context

cosmo0 already has descriptor-backed standard generic support. The revised model does not immediately remove that implementation, but it treats the source-facing APIs as core0/std capabilities. Stage 1 needs only a small subset of these APIs.

## Goals / Non-Goals

**Goals:**

- Specify minimal `Option`, `Result`, and `Vec` APIs in `docs/cosmo0/std.typ`.
- Keep standard generic type applications sealed and stage-controlled.
- Add cosmo1 diagnostics and lexer token buffer code that uses these APIs.
- Add direct and cosmo1 validation tests.

**Non-Goals:**

- Add user-defined generic classes or functions.
- Add full collection APIs, iterators, maps, or sets.
- Remove all existing descriptor lowering paths in this slice.

## Decisions

### Minimal API Only

Stage 1 should use a narrow surface: construction, `Vec.push`, indexed or checked access, length, and variant matching for `Option`/`Result`.

### Std Owns The API

Implementation may temporarily lower through descriptor intrinsics, but the spec and capability model describe these as std APIs.

### Cosmo1 Uses Real Component State

Diagnostics should store a `Vec<Diagnostic>`, and the lexer should produce or buffer `Vec<Token>`. Tests should exercise these through cosmo1 source.

## Risks / Trade-offs

- Risk: std ownership conflicts with existing descriptor names.
  Mitigation: document transitional implementation in `runtime.typ` while `std.typ` owns the API contract.

- Risk: generic support expands accidentally.
  Mitigation: keep user-defined generics explicitly rejected and test for that.

## Migration Plan

1. Update `docs/cosmo0/type.typ`, `class.typ`, and `std.typ`.
2. Add or align std declarations for `Option`, `Result`, and `Vec`.
3. Connect capability validation.
4. Add cosmo1 diagnostic and lexer buffer components.
5. Add tests.

## Open Questions

- Should Stage 1 expose `Vec.get` as a direct value, `Option<T>`, or both?
- Should `Result` error types be ordinary classes or a minimal standard error enum for early I/O?

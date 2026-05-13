## Context

cosmo1 should store compiler data in arenas and refer to nodes with typed IDs. This keeps ownership, mutation, and serialization clearer than pervasive raw pointers.

## Goals / Non-Goals

**Goals:**

- Define `Arena<T>` and `Id<T>` as sealed core0/std types.
- Add allocation, lookup, mutable lookup, and length APIs.
- Add cosmo1 syntax AST arena usage.
- Add tests for type-safe ID usage.

**Non-Goals:**

- Add user-defined generic classes.
- Add garbage collection or arbitrary ownership models.
- Implement all type/name/IR arenas in this slice.

## Decisions

### `Id<T>` Is Phantom-Typed

`Id<T>` should prevent mixing `Id<Expr>` with `Id<Ty>` at source level even if the runtime representation is an integer.

### Arena API Is Minimal

The initial API should support `alloc`, `get`, `get_mut`, and `len`. Iteration and removal can wait.

### Syntax AST Is The First Consumer

`syntax/ast` provides a concrete later-stage consumer without requiring name resolution or type checking.

## Risks / Trade-offs

- Risk: arena APIs expand into full collection design.
  Mitigation: keep map/set and general collection iteration separate.

- Risk: phantom typing requires descriptor-like compiler support.
  Mitigation: document exactly which part is primitive/sealed std behavior in `type.typ` and `std.typ`.

## Migration Plan

1. Update `docs/cosmo0/type.typ` and `std.typ`.
2. Add `Arena<T>` and `Id<T>` std declarations/support.
3. Add `syntax/ast` arena usage.
4. Add tests for ID safety and arena operations.

## Open Questions

- Should `Id<T>` expose numeric conversion for serialization, or keep it opaque initially?
- Should `Arena<T>.get` return a reference or a value copy?

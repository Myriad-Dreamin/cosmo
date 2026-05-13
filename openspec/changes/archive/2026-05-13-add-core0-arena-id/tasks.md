## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/type.typ` with sealed `Arena<T>` and `Id<T>` type application rules.
- [x] 1.2 Update `docs/cosmo0/std.typ` with the minimal arena API.
- [x] 1.3 Update `docs/cosmo0/runtime.typ` with any runtime representation constraints.

## 2. Standard API

- [x] 2.1 Add `Arena<T>` allocation support.
- [x] 2.2 Add `Id<T>` typed ID support.
- [x] 2.3 Add immutable and mutable lookup APIs.
- [x] 2.4 Add length support.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend `syntax/ast.cos` to store syntax nodes in arenas.
- [x] 3.2 Prepare type aliases such as `ExprId` for syntax IDs.

## 4. Tests

- [x] 4.1 Add arena allocation and lookup tests.
- [x] 4.2 Add mutable lookup tests.
- [x] 4.3 Add negative tests for mixing different `Id<T>` types.
- [x] 4.4 Add cosmo1 syntax arena validation tests.

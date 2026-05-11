## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/type.typ` with sealed `Arena<T>` and `Id<T>` type application rules.
- [ ] 1.2 Update `docs/cosmo0/std.typ` with the minimal arena API.
- [ ] 1.3 Update `docs/cosmo0/runtime.typ` with any runtime representation constraints.

## 2. Standard API

- [ ] 2.1 Add `Arena<T>` allocation support.
- [ ] 2.2 Add `Id<T>` typed ID support.
- [ ] 2.3 Add immutable and mutable lookup APIs.
- [ ] 2.4 Add length support.

## 3. Cosmo1 Components

- [ ] 3.1 Add or extend `syntax/ast.cos` to store syntax nodes in arenas.
- [ ] 3.2 Prepare type aliases such as `ExprId` for syntax IDs.

## 4. Tests

- [ ] 4.1 Add arena allocation and lookup tests.
- [ ] 4.2 Add mutable lookup tests.
- [ ] 4.3 Add negative tests for mixing different `Id<T>` types.
- [ ] 4.4 Add cosmo1 syntax arena validation tests.

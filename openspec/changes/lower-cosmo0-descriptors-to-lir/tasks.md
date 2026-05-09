## 1. Descriptor Lowering Model

- [ ] 1.1 Define the mapping from typed descriptor operations to LIR intrinsics or runtime calls.
- [ ] 1.2 Add descriptor lowering metadata for required argument types, result types, mutability, and backend operation names.
- [ ] 1.3 Ensure descriptor-lowered LIR remains independent of full standard-library source implementations.

## 2. Standard Generic Lowering

- [ ] 2.1 Lower `Vec` construction, length, indexing, push, and iteration operations.
- [ ] 2.2 Lower `Option` and `Result` construction, tag checks, payload access, and match support operations.
- [ ] 2.3 Lower `Arena` allocation, `Id` construction/usage, arena get, arena get-mutable, and arena length operations.
- [ ] 2.4 Lower `Map` and `Set` construction, lookup, insertion, key validation results, and deterministic iteration operations.

## 3. Core Runtime Lowering

- [ ] 3.1 Lower scalar and string helper operations needed by compiler-shaped code.
- [ ] 3.2 Lower `StringBuilder` append and finish operations.
- [ ] 3.3 Represent runtime descriptor requirements in LIR so backend emission can include support code exactly where needed.

## 4. Verification And Tests

- [ ] 4.1 Run the LIR type checker on descriptor-lowered modules.
- [ ] 4.2 Add tests for each lowered standard generic and runtime descriptor group.
- [ ] 4.3 Add tests proving descriptor lowering preserves deterministic iteration policy.
- [ ] 4.4 Add negative tests for unsupported descriptor operations reaching the lowering phase.

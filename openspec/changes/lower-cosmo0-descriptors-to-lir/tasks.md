## 1. Descriptor Lowering Model

- [x] 1.1 Define the mapping from typed descriptor operations to LIR intrinsics or runtime calls.
- [x] 1.2 Add descriptor lowering metadata for required argument types, result types, mutability, and backend operation names.
- [x] 1.3 Ensure descriptor-lowered LIR remains independent of full standard-library source implementations.

## 2. Standard Generic Lowering

- [x] 2.1 Lower `Vec` construction, length, indexing, push, and iteration operations.
- [x] 2.2 Lower `Option` and `Result` construction, tag checks, payload access, and match support operations.
- [x] 2.3 Lower `Arena` allocation, `Id` construction/usage, arena get, arena get-mutable, and arena length operations.
- [x] 2.4 Lower `Map` and `Set` construction, lookup, insertion, key validation results, and deterministic iteration operations.

## 3. Core Runtime Lowering

- [x] 3.1 Lower scalar and string helper operations needed by compiler-shaped code.
- [x] 3.2 Lower `StringBuilder` append and finish operations.
- [x] 3.3 Represent runtime descriptor requirements in LIR so backend emission can include support code exactly where needed.

## 4. Verification And Tests

- [x] 4.1 Run the LIR type checker on descriptor-lowered modules.
- [x] 4.2 Add tests for each lowered standard generic and runtime descriptor group.
- [x] 4.3 Add tests proving descriptor lowering preserves deterministic iteration policy.
- [x] 4.4 Add negative tests for unsupported descriptor operations reaching the lowering phase.

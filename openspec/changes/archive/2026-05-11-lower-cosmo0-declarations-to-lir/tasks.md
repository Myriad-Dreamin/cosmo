## 1. Lowering Context

- [x] 1.1 Add a lowering context that maps typed declarations, functions, fields, variants, parameters, and locals to LIR identifiers.
- [x] 1.2 Add deterministic declaration and local ordering for lowered LIR output.
- [x] 1.3 Preserve source origin metadata needed by LIR checker diagnostics.

## 2. Declaration Lowering

- [x] 2.1 Lower typed modules and top-level functions into LIR module declarations.
- [x] 2.2 Lower classes and fields into LIR layout declarations.
- [x] 2.3 Lower function and method signatures, including receiver parameters for `self`, `&self`, and `&mut self`.
- [x] 2.4 Lower simple type aliases into the type information consumed by LIR checking.

## 3. Function Body Lowering

- [x] 3.1 Lower parameter binding, local declarations, local reads, local writes, assignments, direct calls, field get/set, and returns.
- [x] 3.2 Lower simple expression blocks into linear LIR operation sequences.
- [x] 3.3 Emit explicit LIR locals for intermediate values where typed expressions require them.

## 4. Verification And Tests

- [x] 4.1 Run the LIR type checker on declaration-lowered modules.
- [x] 4.2 Add tests for lowered top-level functions, methods, fields, assignments, calls, and returns.
- [x] 4.3 Add negative tests proving invalid typed input or invalid lowering output is diagnosed before backend emission.

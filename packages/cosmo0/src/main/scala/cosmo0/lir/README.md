# lir

LIR is the lowered, backend-facing intermediate representation.

`Lir.scala` defines the LIR model, constructors, and debug renderer.
`Lowerer.scala` converts `TypedModule` into `LirModule`, including function
builders, blocks, branches, locals, descriptor intrinsics, variants, borrows,
and trusted extern calls.

`TypeChecker.scala` validates LIR shape and type correctness. The C++ backend
uses it at the compile boundary, so hand-written LIR tests and lowered modules
share the same safety checks.

Upstream input comes from `syntax/Typed.scala` and `tyck/Typer.scala`.
Downstream output goes to `backend/cpp/Backend.scala`.

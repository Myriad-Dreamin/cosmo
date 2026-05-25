# syntax

Syntax owns the source-facing representation before type checking.

`Parser.scala` parses source text through the shared `cosmo.syntax` parser.
`Elaborator.scala` is the source-to-cosmo0 bridge: it rejects unsupported full
language constructs, preserves spans, collects file-level C/C++ extern metadata,
and produces `UntypedModule`.

`Untyped.scala`, `Typed.scala`, and `Types.scala` define the cosmo0 syntax and
source type data passed between stages. `ExternAbi.scala` defines trusted extern
bindings shared by elaboration, type checking, lowering, and the C++ backend.

Downstream flow:

- `UntypedModule` is consumed by `tyck/mltt/Typer.scala`.
- `TypedModule` is consumed by `lir/Lowerer.scala`.
- `SourceType` and extern ABI data are shared by `tyck/`, `lir/`, and
  `backend/`.

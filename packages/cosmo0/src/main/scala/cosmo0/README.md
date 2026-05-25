# cosmo0

This directory is the public compiler API surface plus the top-level
component map. The Scala declarations currently remain in `package cosmo0`;
the subdirectories are physical ownership boundaries, not Scala namespace
boundaries.

`Cosmo0.scala` is the facade used by tests, Scala.js exports, and host callers.
`source/Model.scala` contains shared result, diagnostic, source, and package
models.

The normal single-module pipeline is:

1. `syntax/Parser.scala` parses `SourceFile` text into parser syntax.
2. `syntax/Elaborator.scala` converts parser syntax into `UntypedModule`.
3. `tyck/mltt/Typer.scala` checks `UntypedModule` into `TypedModule`.
4. `lir/Lowerer.scala` lowers `TypedModule` into `LirModule`.
5. `lir/TypeChecker.scala` validates hand-written and lowered LIR.
6. `backend/cpp/Backend.scala` emits C++ from checked LIR.

The package pipeline in `source/` reuses the same stages after loading package
metadata, source files, and module import order.

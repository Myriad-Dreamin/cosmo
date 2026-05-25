# source

This directory owns shared cosmo0 source models plus package loading and
package-level compilation.

`Model.scala` contains shared result, diagnostic, source, and package models.
`Pipeline.scala` reads `cosmo.json`, discovers source files, loads
dependencies, builds the module import graph, enforces package visibility,
combines ordered modules, and then reuses the normal compiler stages.

Package flow:

1. Load metadata and source files into `Cosmo0Package`.
2. Elaborate each module with `syntax/Elaborator.scala`.
3. Validate duplicate modules, duplicate declarations, imports, visibility, and
   import cycles.
4. Combine declarations in topological order.
5. Type-check with `tyck/Typer.scala`.
6. Lower with `lir/Lowerer.scala`.
7. Compile with `backend/cpp/Backend.scala` when requested by the facade.

The public package entry points live on `Cosmo0.scala`; this directory keeps the
implementation details out of the API facade.

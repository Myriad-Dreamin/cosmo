# tyck

Type checking owns source-level validation after elaboration and before LIR.

`Typer.scala` checks `UntypedModule` into `TypedModule`. It resolves names,
aliases, classes, traits, impls, standard generic descriptors, method calls,
patterns, mutability, and expression result types.

`Profiles.scala` describes checker profiles and feature gates. `StageCapabilities.scala`
validates package-level stage capability profiles before package checking.

Subdirectories:

- `dependent/` contains the Scala mirror for dependent-pattern experiments.
- `mltt/` contains the Scala mirror for the MLTT core checker experiments.

Upstream input comes from `syntax/Elaborator.scala`. Downstream output goes to
`lir/Lowerer.scala`. Profile metadata is also read by `package/Pipeline.scala`
when package metadata selects a checker profile.

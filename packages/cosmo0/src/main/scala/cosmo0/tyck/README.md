# tyck

Type checking owns source-level validation after elaboration and before LIR.

`mltt/Typer.scala` checks `UntypedModule` into `TypedModule`. It resolves names,
aliases, classes, traits, impls, standard generic descriptors, method calls,
patterns, mutability, and expression result types, while delegating type
relations and dependent-pattern hooks to `MlttTypeChecker`.

`Profiles.scala` describes checker profiles and feature gates. `StageCapabilities.scala`
validates package-level stage capability profiles before package checking.
`MlttTypeChecker` owns MLTT source-type relations plus `mltt.core` and
`mltt.dependent-patterns` source directives; it calls `DependentPatterns` for
dependent-pattern elaboration.

Subdirectories:

- `dependent/` contains the Scala mirror for dependent-pattern experiments.
- `mltt/` contains the MLTT-backed source typer and Scala mirror for the MLTT
  core checker experiments.

Upstream input comes from `syntax/Elaborator.scala`. Downstream output goes to
`lir/Lowerer.scala`. Profile metadata is also read by `source/Pipeline.scala`
when package metadata selects a checker profile. Ordinary source flows through
`MlttTyper`; MLTT assertion directives are routed directly through
`MlttTypeChecker`, and dependent-patterns are handled as an MLTT extension.

# mltt

This directory contains the MLTT-backed source typer and the Scala mirror for
the experimental MLTT core checker.

`TypeChecker.scala` defines MLTT terms, contexts, declaration metadata,
metavariables, conversion, bidirectional infer/check entry points, and
diagnostics.

`Typer.scala` is the ordinary cosmo0 source frontend. It keeps source
declaration/expression elaboration in the source AST, but its type equality,
assignability, and indexed match hooks call `MlttTypeChecker`.

`MlttTypeChecker.checkSource` and `MlttTypeChecker.checkSources` let
`checkerProfile: "mltt.core"` and `checkerProfile: "mltt.dependent-patterns"`
source fixtures execute named MLTT assertions by constructing core terms inside
this checker. The dependent-pattern profile invokes `DependentPatterns` as an
MLTT extension rather than as a separate checker path.

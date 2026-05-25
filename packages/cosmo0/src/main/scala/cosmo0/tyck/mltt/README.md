# mltt

This directory contains the Scala mirror for the experimental MLTT core checker.

`TypeChecker.scala` defines MLTT terms, contexts, declaration metadata,
metavariables, conversion, bidirectional infer/check entry points, and
diagnostics.

It is profile-gated and separate from the default cosmo0 source checker.
`MlttTypeChecker.checkSource` and `MlttTypeChecker.checkSources` let
`checkerProfile: "mltt.core"` and `checkerProfile: "mltt.dependent-patterns"`
source fixtures execute named MLTT assertions by constructing core terms inside
this checker. The dependent-pattern profile invokes `DependentPatterns` as an
MLTT extension. The ordinary `cosmo0.subset` source path still does not
elaborate general user source into MLTT core.

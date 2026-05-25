# dependent

This directory contains dependent-pattern support used by experimental checker
profiles and tests.

`Patterns.scala` models dependent pattern terms, constructors, source patterns,
case trees, branch refinements, unification results, and diagnostics.

`MlttTypeChecker` calls `DependentPatterns` when the selected profile is
`mltt.dependent-patterns`, so named dependent-pattern assertions and indexed
source-match hooks are MLTT extension obligations rather than a separate
checker path. Ordinary package checking flows through `tyck/mltt/Typer.scala`.

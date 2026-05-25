# dependent

This directory contains dependent-pattern support used by experimental checker
profiles and tests.

`Patterns.scala` models dependent pattern terms, constructors, source patterns,
case trees, branch refinements, unification results, and diagnostics.

It is intentionally isolated from the ordinary cosmo0 subset path.
`MlttTypeChecker` calls `DependentPatterns` when the selected profile is
`mltt.dependent-patterns`, so named dependent-pattern assertions are MLTT
extension obligations rather than a separate checker path. Ordinary
`cosmo0.subset` package checking still flows through `tyck/Typer.scala`.

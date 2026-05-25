# dependent

This directory contains dependent-pattern support used by experimental checker
profiles and tests.

`Patterns.scala` models dependent pattern terms, constructors, source patterns,
case trees, branch refinements, unification results, and diagnostics.

It is intentionally isolated from the ordinary cosmo0 subset path.
`DependentPatterns.checkSource` and `DependentPatterns.checkSources` let
`checkerProfile: "mltt.dependent-patterns"` source fixtures execute named
dependent-pattern assertions by constructing source clauses inside this
elaborator. Ordinary `cosmo0.subset` package checking still flows through
`tyck/Typer.scala`.

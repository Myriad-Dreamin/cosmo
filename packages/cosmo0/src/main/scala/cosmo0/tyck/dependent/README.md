# dependent

This directory contains dependent-pattern support used by experimental checker
profiles and tests.

`Patterns.scala` models dependent pattern terms, constructors, source patterns,
case trees, branch refinements, unification results, and diagnostics.

It is intentionally isolated from the ordinary cosmo0 subset path. The default
source pipeline reaches this code only through explicit checker profile support
and tests; ordinary package checking still flows through `tyck/Typer.scala`.

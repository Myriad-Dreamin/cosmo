## Context

cosmo0 is intended to become a separate Scala-implemented compiler path for the
bootstrap-oriented Cosmo subset. The current repository only has the existing
full `cosmo` Scala.js project, whose parser, typer, evaluator, and C++ codegen
are coupled through the `cosmo` package namespace.

This change creates the package boundary before later cosmo0 phases exist. The
boundary should be executable enough for tests and future integration work, but
it must not change full compiler behavior.

## Goals / Non-Goals

**Goals:**

- Add an independent Scala.js `cosmo0` sbt project under `packages/cosmo0`.
- Reuse the shared parser for the initial parse entry point.
- Define public result, diagnostic, source file, source span, and phase status
  types that later phases can share.
- Return structured pending or unsupported results for unimplemented check and
  compile phases.
- Keep existing `cosmo` build targets and compiler entry points intact.

**Non-Goals:**

- Implement cosmo0 subset checking, typing, LIR lowering, or C++ emission.
- Split the existing parser into a separate sbt project.
- Change accepted behavior for the current full compiler pipeline.

## Decisions

### Add `cosmo0` as a separate Scala.js project

The new sbt project lives at `packages/cosmo0` and uses the `cosmo0` Scala
package namespace. This keeps bootstrap-specific APIs out of the current full
compiler namespace and gives later phases an independent test target.

Alternative considered: add cosmo0 files inside `packages/cosmo`. That would be
faster initially, but it would make later bootstrap work harder to keep isolated
from full compiler behavior.

### Depend on the existing `cosmo` project for parsing

The facade calls the shared `cosmo.Parser.root` parser directly. This avoids
creating a second grammar and makes the parser dependency explicit through the
sbt project relationship.

Alternative considered: copy or wrap parser code in the new package. That would
create immediate syntax drift risk without adding useful behavior.

### Use structured phase results from the first API

`Cosmo0Result` carries a phase, status, optional value, and diagnostics. Parse
can succeed or fail today. Check returns `Pending`, and compile returns
`Unsupported`, because those phases do not exist yet and should not pretend to
produce completed outputs.

Alternative considered: throw `NotImplementedError` from check and compile. That
would make the facade harder to use in tests and future tooling because callers
would need exception handling for expected phase availability.

## Risks / Trade-offs

- The parser dependency currently brings in the full `cosmo` project rather than
  a parser-only module. This is acceptable for the initial package boundary, and
  a future parser split can keep the `cosmo0` facade API unchanged.
- Pending and unsupported statuses are intentionally conservative. Later changes
  must replace those results with real checked and compiled values as phases are
  implemented.

## Migration Plan

1. Add the `cosmo0` project and package namespace.
2. Add the facade and shared structured result types.
3. Add smoke tests for instantiation, parsing, pending check, and unsupported
   compile behavior.
4. Replace pending check behavior with a real subset checker in a later change.
5. Replace unsupported compile behavior after typed expressions, LIR, and backend
   phases exist.

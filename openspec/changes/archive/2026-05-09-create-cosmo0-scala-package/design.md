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
- Move the shared parser boundary into `cosmo0` for the initial parse entry
  point and for the existing full compiler path.
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

The new sbt project lives at `packages/cosmo0`. Public cosmo0 compiler APIs use
the `cosmo0` Scala package namespace. The parser AST remains under
`cosmo.syntax` as a compatibility boundary so the existing full compiler can
continue to consume the same syntax node types while the source files are owned
by the `cosmo0` project.

Alternative considered: add cosmo0 files inside `packages/cosmo`. That would be
faster initially, but it would make later bootstrap work harder to keep isolated
from full compiler behavior.

### Make `cosmo` depend on `cosmo0` for parsing

The parser now lives at `cosmo0.Parser`, and the existing full compiler imports
that parser. This avoids creating a second grammar and makes cosmo0 the owner of
the first parse boundary used by later bootstrap phases.

Alternative considered: keep `Parser` in `packages/cosmo` and let cosmo0 depend
on the full compiler project. That would invert the intended layering and force
the bootstrap path to depend on typer, evaluator, codegen, and runtime pieces it
does not need.

### Use structured phase results from the first API

`Cosmo0Result` carries a phase, status, optional value, and diagnostics. Parse
can succeed or fail today. Check returns `Pending`, and compile returns
`Unsupported`, because those phases do not exist yet and should not pretend to
produce completed outputs.

Alternative considered: throw `NotImplementedError` from check and compile. That
would make the facade harder to use in tests and future tooling because callers
would need exception handling for expected phase availability.

## Risks / Trade-offs

- The parser AST package name remains `cosmo.syntax` for compatibility even
  though the sources are now compiled by `cosmo0`. A later cleanup can rename the
  syntax namespace once the full compiler is ready for broader import churn.
- Pending and unsupported statuses are intentionally conservative. Later changes
  must replace those results with real checked and compiled values as phases are
  implemented.

## Migration Plan

1. Add the `cosmo0` project and package namespace.
2. Move `Parser` and parser-owned syntax nodes into the `cosmo0` project.
3. Make `cosmo` depend on `cosmo0` and import `cosmo0.Parser`.
4. Add the facade and shared structured result types.
5. Add smoke tests for instantiation, parsing, pending check, and unsupported
   compile behavior.
6. Replace pending check behavior with a real subset checker in a later change.
7. Replace unsupported compile behavior after typed expressions, LIR, and backend
   phases exist.

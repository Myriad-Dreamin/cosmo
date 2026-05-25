# Agent Instructions

## Code Style

- Prefer flat, linear control flow. Use guard clauses, early returns, small helper functions, and explicit intermediate values instead of deeply nested `if`, `match`, callback, or result-unwrapping pyramids.
- When handling `Option`, `Result`, or similar sum types, keep the happy path readable at the top level. Push repeated error propagation or validation details into focused helpers instead of stacking nested branches.
- Avoid "noodle" code: if indentation depth keeps growing or a function becomes difficult to scan from top to bottom, refactor before committing.
- Keep functions small enough that their purpose and failure paths are obvious. Splitting parsing, validation, conversion, and construction into separate helpers is preferred over one large nested function.
- If language or bootstrap limitations force nested control flow, keep the nesting narrow and localized, and prefer returning immediately from error cases where the language supports it.

## Scala Formatting

- Before handing off any Scala change, run `scripts/check-scala-style.sh` from the repository root and fix any failure.
- If the Scala formatting check fails, run scalafmt on the same root that the check covers, then run `scripts/check-scala-style.sh` again. The default checked root is `packages/cosmo0/src/main/scala/cosmo0`.
- When editing Scala outside the default checked root, set `SCALA_STYLE_ROOT` to the edited Scala source root and run the same check before finishing.

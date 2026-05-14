## Context

The parser self-compile source uses top-level helpers, class methods, fields, parameters, locals, and `self`. A stable resolver avoids duplicating lookup logic in the type checker.

## Goals / Non-Goals

**Goals:**

- Intern symbols deterministically.
- Collect definitions from syntax.
- Resolve names and member lookup inputs for the parser subset.

**Non-Goals:**

- Trait resolution.
- General overload resolution.
- Macro or staged name lookup.

## Decisions

- Separate declaration collection from expression name resolution.
- Represent fields and methods as definitions owned by class definitions.
- Treat unresolved names as diagnostics, not lowering-time failures.

## Risks / Trade-offs

- Risk: member lookup overlaps with type checking. Mitigation: resolver records candidate definitions while type checking validates receiver type and call shape.

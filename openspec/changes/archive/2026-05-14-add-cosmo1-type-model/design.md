## Context

The parser self-compile subset uses primitive types, user classes, function signatures, references, and `Unit`/`Bool`/`String`/`usize`/`u8`. A small type model should support those without importing full Cosmo type-system complexity.

## Goals / Non-Goals

**Goals:**

- Define stable type ids and type constructors.
- Compare and display types deterministically.
- Provide diagnostic data for later type-checking failures.

**Non-Goals:**

- User-defined generic checking.
- Trait solving.
- Compile-time `Type` values.

## Decisions

- Keep type equality structural for parser-source types.
- Model references directly in cosmo1 type data.
- Reserve error and never types for later control-flow and recovery.

## Risks / Trade-offs

- Risk: type model grows toward full Cosmo too early. Mitigation: only include forms required by current parser-source acceptance and future proposals must add new forms explicitly.

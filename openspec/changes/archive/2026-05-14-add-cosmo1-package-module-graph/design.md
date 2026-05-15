## Context

`parser.cos` can be validated as a single source first, but cosmo1 eventually needs package metadata and import ordering. This proposal establishes the package boundary early without requiring full package self-hosting.

## Goals / Non-Goals

**Goals:**

- Load the package metadata needed by cosmo1 validation slices.
- Resolve listed source files and imports deterministically.
- Report missing import and cycle diagnostics.

**Non-Goals:**

- Full package manager behavior.
- Linker or artifact writing.
- Full `packages/cosmoc` self-hosting.

## Decisions

- Support explicit source lists before recursive package scanning.
- Use deterministic topological ordering.
- Keep import validation independent from type checking.

## Risks / Trade-offs

- Risk: package graph work arrives before the single-file parser path needs it. Mitigation: keep implementation small and allow single-file validation to bypass package metadata until ready.

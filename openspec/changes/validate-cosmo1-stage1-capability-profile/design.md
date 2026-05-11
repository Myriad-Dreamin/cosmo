## Context

Earlier proposals add individual capabilities. This proposal is the integration slice for Stage 1: source loading, spans, source maps, diagnostics, tokens, lexing, and text output should validate as a package.

## Goals / Non-Goals

**Goals:**

- Define the Stage 1 package layout and capability profile.
- Add Stage 1 source files that use only allowed cosmo0 constructs.
- Run check-package and compile-package validation through cosmo0.
- Add negative tests for unsupported full-language features.

**Non-Goals:**

- Add JSON AST loading.
- Add native parser beyond lexing.
- Add package graph, name resolution, type checking, or codegen slices.

## Decisions

### Stage 1 Is A Package Validation Target

Validation should run through the package pipeline, not isolated file-only fixtures, so imports, metadata, capability profiles, and backend output are exercised together.

### Allowed Feature Set Is Explicit

Stage 1 can use classes, variants, methods, mutation, references, control flow, primitive descriptors, and declared Stage 1 std capabilities. It cannot use user generics, host `Type`, reflection, staging, closures, or unsupported higher-order APIs.

### Deterministic Output Is Required

Generated output and diagnostic rendering must be stable so Stage 1 can become a regression target.

## Risks / Trade-offs

- Risk: Stage 1 accidentally depends on later capabilities.
  Mitigation: validation profile excludes JSON, command, arena IDs, map/set, and big-number support.

- Risk: integration hides which capability failed.
  Mitigation: keep direct capability tests in the earlier proposals and use clear missing-capability diagnostics here.

## Migration Plan

1. Update `docs/cosmo0/spec.typ`, `package.typ`, and `testing.typ`.
2. Add Stage 1 package metadata and source files.
3. Add package check validation.
4. Add compile validation through the C++ backend.
5. Add deterministic and negative tests.

## Open Questions

- Should Stage 1 smoke input be a checked-in `.cos` file or generated inside the test?
- Should token output be printed, snapshot-tested, or asserted structurally?

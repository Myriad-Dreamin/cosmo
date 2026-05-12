## Context

cosmo0 currently has implementation and OpenSpec changes, but it does not have a dedicated language/runtime model under `docs/cosmo0/`. The staged runtime plan depends on clear boundaries between primitive descriptors, core0 standard APIs, packages, and cosmo1 validation stages.

## Goals / Non-Goals

**Goals:**

- Create the `docs/cosmo0/` spec skeleton.
- Define which file owns each part of the cosmo0 model.
- Document the rule for synchronizing compiler bug fixes with spec updates and regression tests.
- Add enough Stage 1 profile scaffolding for later capability proposals to reference.

**Non-Goals:**

- Specify every cosmo0 behavior completely in this first slice.
- Change compiler implementation.
- Add runtime descriptors or standard APIs.

## Decisions

### Use Focused Spec Files

The docs skeleton will be split by behavior area rather than by implementation module:

- `spec.typ` for index, goals, subset boundary, and conformance rules.
- `type.typ` for primitive types, references, aliases, and standard type application.
- `class.typ` for class, field, method, constructor, and variant subset rules.
- `expr.typ` for expression typing and literal/call/selection rules.
- `control-flow.typ` for accepted control-flow forms.
- `std.typ` for core0 standard interfaces and capability identifiers.
- `runtime.typ` for primitive descriptors, extern ABI hooks, and backend runtime requirements.
- `package.typ` for package metadata, imports, source loading, and stage validation.
- `testing.typ` for positive, negative, deterministic, and bug regression tests.

Alternative considered: one large spec file. That would be faster to create, but it would make future PR reviews harder because every change would touch the same file.

### Make Bug/Spec Sync Explicit

If implementation behavior conflicts with the current spec, the implementation is fixed and regression tests are added. If the intended behavior is missing or wrong in the spec, the same PR updates the spec before relying on it from cosmo1 source.

Alternative considered: defer spec cleanup until after implementation. That risks turning implementation quirks into bootstrap requirements.

### Keep Validation Lightweight

The first validation should check for the doc structure and task references. It should not require a full Typst build if the repository does not yet have a stable docs command.

## Risks / Trade-offs

- Risk: the skeleton creates empty docs that look authoritative.
  Mitigation: each file should state which sections are normative now and which are placeholders.

- Risk: future implementation PRs skip spec updates.
  Mitigation: later staged runtime proposals must name changed spec files or justify implementation-only status.

## Migration Plan

1. Add the docs skeleton.
2. Add initial bug/spec sync text to `testing.typ`.
3. Add Stage 1 capability profile placeholders in `spec.typ`, `std.typ`, and `package.typ`.
4. Add lightweight validation.

## Open Questions

- Which command should become the canonical docs build target once `docs/cosmo0/` contains real Typst content?
- Should OpenSpec validation enforce references to `docs/cosmo0/` automatically or only through review?

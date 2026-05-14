## Context

The current Scala parser can provide JSON AST output. A cosmo1 JSON loader lets later bootstrap stages consume that format before the native parser is complete.

## Goals / Non-Goals

**Goals:**

- Define minimal JSON value and parse APIs in core0/std.
- Support object fields, arrays, strings, booleans, nulls, and numeric literal text or value access.
- Add `syntax/json_loader` for selected samples.
- Add tests through parser JSON fixtures.

**Non-Goals:**

- Build a full JSON library.
- Make JSON a descriptor family.
- Stabilize every current parser JSON detail forever.

## Decisions

### JSON Is A Bridge Capability

`core0.json` is transitional but useful. It should be clearly staged and not required for Stage 1.

### Accessors Are Fallible

Object and array access should return `Option` or `Result` values so loader errors can become diagnostics.

### Loader Owns AST Shape

`syntax/json_loader` maps JSON into cosmo1 AST data. The standard JSON API should not know about AST structure.

## Risks / Trade-offs

- Risk: current parser JSON becomes accidentally permanent.
  Mitigation: document selected fixture compatibility and keep broader format decisions in cosmo1 syntax specs.

- Risk: numeric JSON values need big-number support.
  Mitigation: preserve literal text where precision matters and defer big-number arithmetic to a later capability.

## Migration Plan

1. Update `docs/cosmo0/std.typ` and `runtime.typ`.
2. Add JSON std declarations and extern parse binding.
3. Add selected parser JSON fixtures.
4. Add `syntax/json_loader` and tests.

## Open Questions

- Should JSON numbers expose raw text, typed numeric values, or both?
- Which parser JSON sample should be the first compatibility target?

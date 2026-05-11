## Context

Compiler output, diagnostics, and snapshots must be stable. Maps and sets that affect output order need deterministic semantics or explicit ordering APIs.

## Goals / Non-Goals

**Goals:**

- Define deterministic map/set API and iteration behavior.
- Define supported key types until trait-based hashing/ordering exists.
- Add a cosmo1 component that uses deterministic keyed collections.
- Add deterministic and negative tests.

**Non-Goals:**

- Add general hash/ordering traits.
- Add every collection method.
- Optimize map/set performance before semantics are stable.

## Decisions

### Deterministic By Contract

Either `Map`/`Set` are deterministic by default or the API uses explicit `OrderedMap`/`OrderedSet` names. The proposal must choose and document the rule.

### Key Types Are Restricted

Until trait solving exists, allowed key types should be explicitly registered standard types such as strings, symbols, primitive integers, or typed IDs.

### Name/Package Data Is The First Consumer

Package graph or scope data gives deterministic maps a real output-affecting use case.

## Risks / Trade-offs

- Risk: deterministic collections are slower.
  Mitigation: prioritize stable compiler output first; add non-deterministic collections later only for non-output-affecting paths.

- Risk: key restrictions are too narrow.
  Mitigation: expand by explicit std capability changes and tests.

## Migration Plan

1. Update `docs/cosmo0/std.typ` with deterministic collection semantics.
2. Add map/set declarations and supported key checks.
3. Add cosmo1 package/name component usage.
4. Add deterministic output and key rejection tests.

## Open Questions

- Should the public names be `Map`/`Set` or `OrderedMap`/`OrderedSet`?
- Should symbols be a separate std type before map/set support lands?

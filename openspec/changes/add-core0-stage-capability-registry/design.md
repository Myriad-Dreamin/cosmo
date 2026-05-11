## Context

The revised model separates primitive descriptors from core0 standard capabilities. A validation request needs to know which subset is required for each cosmo1 stage.

## Goals / Non-Goals

**Goals:**

- Define capability identifiers such as `core0.text`, `core0.path-fs`, and `core0.char-class`.
- Define the Stage 1 profile.
- Validate required primitive descriptors, std capabilities, and extern bindings.
- Add clear diagnostics for missing capabilities.

**Non-Goals:**

- Implement every Stage 1 std capability.
- Build the complete cosmo1 package.
- Add JSON, command, arena, map/set, or big-number support.

## Decisions

### Profiles Contain Primitive And Std Requirements

A stage profile contains primitive descriptor requirements and core0/std capability requirements. This keeps primitive compiler support visible without turning the profile into a descriptor list.

### Later Capabilities Are Explicitly Optional

Stage 1 must not require `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, or `core0.big-number`.

### Validation Runs Before Lowering

Missing capabilities should be reported during package/source validation before descriptor lowering or backend emission produces less specific errors.

## Risks / Trade-offs

- Risk: capability names become too coarse.
  Mitigation: start with Stage 1 needs and split identifiers when tests reveal ambiguous diagnostics.

- Risk: profiles drift from cosmo1 source.
  Mitigation: cosmo1 validation tests must compile source that actually uses the declared capabilities.

## Migration Plan

1. Update `docs/cosmo0/spec.typ`, `std.typ`, `package.typ`, and `testing.typ`.
2. Add capability identifiers and profile data.
3. Add validation and diagnostics.
4. Add Stage 1 package metadata/profile scaffolding.

## Open Questions

- Should capability identifiers be user-facing package metadata or internal compiler profile names?
- Should stages be named by cosmo1 milestone or by required capability set?

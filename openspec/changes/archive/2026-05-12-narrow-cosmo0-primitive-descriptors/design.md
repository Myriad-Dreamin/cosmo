## Context

Existing cosmo0 descriptor support covers standard generics and runtime-facing operations. The revised runtime plan keeps only primitive/intrinsic behavior in descriptors and routes ordinary APIs through std interfaces or extern-backed std modules.

## Goals / Non-Goals

**Goals:**

- Define and enforce a primitive descriptor whitelist.
- Document the descriptor boundary in `docs/cosmo0/type.typ`, `expr.typ`, and `runtime.typ`.
- Add cosmo1 span and token smoke components that prove primitive descriptors are enough for early source and lexing data.
- Add tests for accepted primitive descriptor behavior and rejected non-primitive descriptor expansion.

**Non-Goals:**

- Remove temporary implementation details required by existing lowered standard generics in one step.
- Add JSON, filesystem, command, or text builder APIs.
- Introduce user-defined generics or trait solving.

## Decisions

### Whitelist Primitive Descriptors

The intended primitive descriptor set includes `Unit`, `Bool`, low-level scalar literal backing, `usize`, `Byte`, `Char`, references, mutable references, branch conditions, equality/comparison intrinsics, and required ABI hooks.

Alternative considered: keep the descriptor registry open and police additions by review. That makes staged runtime growth too easy to route through compiler internals.

### Reject Ordinary Runtime APIs As Descriptor Families

JSON, filesystem, command execution, string builders, deterministic collections, and arbitrary-precision numbers should be rejected as descriptor families. They may use extern runtime bindings behind std APIs.

### Use Cosmo1 Smoke Components

`source/span.cos` and basic `lex/token.cos` give the primitive boundary a concrete consumer without depending on text, filesystem, or collections.

## Risks / Trade-offs

- Risk: current implementation still uses descriptor paths for standard generics.
  Mitigation: document temporary compatibility and keep the new rule focused on new runtime capability growth.

- Risk: primitive boundary is too narrow.
  Mitigation: future proposals may add primitive descriptors only by updating `runtime.typ`, implementation, and negative tests.

## Migration Plan

1. Document primitive descriptors in `docs/cosmo0/runtime.typ`.
2. Add enforcement or validation for descriptor additions.
3. Add span/token smoke components.
4. Add positive and negative tests.

## Open Questions

- Which existing standard generic lowering paths should be considered temporary descriptor compatibility?
- Should scalar numeric operations be primitive descriptors or ordinary compiler-known typed operations?

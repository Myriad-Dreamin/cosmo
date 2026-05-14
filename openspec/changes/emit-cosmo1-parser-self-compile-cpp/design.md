## Context

cosmo0 already emits C++ for `parser.cos`. This proposal requires cosmo1 to do the same through its own syntax, resolution, type-checking, and lowering pipeline.

## Goals / Non-Goals

**Goals:**

- Emit deterministic C++ from verified parser IR.
- Validate generated C++ with the configured C++ toolchain.
- Prove the cosmo0-compiled cosmo1 pipeline can compile `parser.cos`.

**Non-Goals:**

- Linker/build-system integration.
- Runtime fixture execution beyond generated C++ acceptance unless explicitly added.
- Full `packages/cosmoc` package self-hosting.

## Decisions

- Use verified IR as the only input to C++ emission.
- Keep symbol naming deterministic.
- Reuse parser-required runtime conventions before introducing broader cosmo1 runtime metadata.

## Risks / Trade-offs

- Risk: C++ emission duplicates cosmo0 backend behavior. Mitigation: target the parser self-compile subset first and keep the emitter contract tied to cosmo1 IR, not cosmo0 internals.

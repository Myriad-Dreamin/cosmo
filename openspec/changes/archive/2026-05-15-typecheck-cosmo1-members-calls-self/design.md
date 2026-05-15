## Context

The parser source uses `self.offset`, `self.source.len()`, `self.current_byte()`, `is_space(...)`, and mutable receiver methods. These rules are the main difference between toy expression checking and parser-source checking.

## Goals / Non-Goals

**Goals:**

- Validate fields and method calls against resolved class definitions.
- Validate receiver mutability.
- Provide parser-required `String` intrinsic signatures.

**Non-Goals:**

- Trait dispatch.
- User overload sets.
- Generic method inference.

## Decisions

- Resolve methods to concrete function definitions before lowering.
- Treat `String` intrinsics as compiler-known standard signatures for this slice.
- Check `and`/`or` as Bool operators while leaving short-circuit lowering to IR work.

## Risks / Trade-offs

- Risk: standard intrinsic modeling leaks into full std design. Mitigation: include only parser-required signatures and require future proposals for additional APIs.

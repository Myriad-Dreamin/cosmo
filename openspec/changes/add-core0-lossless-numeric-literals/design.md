## Context

Numeric literals can exceed primitive ranges. Early lexing should not lose information just because arbitrary-precision arithmetic is not available yet.

## Goals / Non-Goals

**Goals:**

- Preserve raw numeric literal text in Stage 1 token data.
- Define when primitive parsing is allowed.
- Prepare later `BigInt`/`BigDecimal` std APIs.
- Add cosmo1 token/literal usage and tests.

**Non-Goals:**

- Implement arbitrary-precision arithmetic in Stage 1.
- Make big numbers primitive descriptors.
- Finish type checking or compile-time evaluation.

## Decisions

### Raw Text Comes First

Lexer/parser data should preserve raw literal text. Later stages can parse into `BigInt`, `BigDecimal`, or primitive values when the required capability exists.

### Big Numbers Are Std Capabilities

`BigInt` and `BigDecimal` should be standard modules/classes with runtime or source implementation, not descriptor families.

### Stage 1 Does Not Require Big Numbers

Stage 1 may tokenize numeric text but must not require `core0.big-number`.

## Risks / Trade-offs

- Risk: preserving raw text delays validation errors.
  Mitigation: Stage 1 can report lexical format errors while deferring range and arithmetic checks.

- Risk: later APIs need different numeric representation.
  Mitigation: raw text is representation-neutral.

## Migration Plan

1. Update `docs/cosmo0/expr.typ` and `std.typ`.
2. Add numeric token raw-text preservation.
3. Add later big-number capability placeholders.
4. Add tests for large literals and Stage 1 independence from big-number support.

## Open Questions

- Should numeric token text include separators and radix prefixes exactly as written?
- Should primitive parse helpers return `Result` with diagnostics-friendly errors?

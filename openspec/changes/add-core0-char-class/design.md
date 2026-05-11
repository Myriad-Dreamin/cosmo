## Context

Lexer code should not inline every character test or rely on descriptor-specific operations. A small std module can provide the classification rules and leave broader Unicode support for later stages.

## Goals / Non-Goals

**Goals:**

- Define lexer-oriented character helper APIs.
- Specify supported character semantics for Stage 1.
- Add cosmo1 lexer scanning that uses the helpers.
- Add direct helper tests and tokenization fixtures.

**Non-Goals:**

- Add complete Unicode identifier rules.
- Add parser logic beyond token scanning.
- Add text storage or filesystem APIs.

## Decisions

### ASCII First If Needed

If Stage 1 only supports ASCII source syntax, `std.typ` should say so. Non-ASCII behavior should be rejected or left explicitly unsupported with tests.

### Helpers Are Ordinary Std Functions

Helpers are functions over `Char` or `Byte`, surfaced through `core0.char-class`.

### Lexer Uses Helpers Directly

`lex/lexer.cos` should call the standard helpers so tests prove the capability is useful from cosmo1 source.

## Risks / Trade-offs

- Risk: ASCII-only behavior becomes permanent by accident.
  Mitigation: document it as Stage 1 scope and leave Unicode as a later extension.

- Risk: helper API duplicates future full std character APIs.
  Mitigation: keep names and semantics small enough to map forward.

## Migration Plan

1. Update `docs/cosmo0/expr.typ` and `std.typ`.
2. Add char-class std helpers.
3. Add lexer scanning logic.
4. Add classification and tokenization tests.

## Open Questions

- Should helpers operate on `Char`, `Byte`, or both?
- Should unsupported non-ASCII characters become diagnostics or raw token errors in Stage 1?

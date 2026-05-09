# cosmo1 Core Acceptance Corpus

This directory contains the first cosmo1-shaped source corpus for the future
cosmo0 pipeline. The corpus is a design and acceptance target, not a working
compiler implementation.

## Path

The corpus lives under `fixtures/cosmo0/cosmo1/core-acceptance/` because it is
cosmo1-style source that will be validated by cosmo0 phases as they become
available.

## Conventions

- Standard generic application uses the parser-supported compile-time spelling:
  `Vec[Token]`, `Option[Span]`, `Result[ExprId, Diagnostic]`, `Map[Symbol, DefId]`,
  `Set[Symbol]`, `Arena[Expr]`, and `Id[Expr]`.
- Descriptor-backed standard values use the same application as constructor
  targets, for example `Vec[DiagnosticLabel]()` and `Option[Token]::Some(token)`.
- References are written as `&T` and `&mut T` in parameter positions, with
  `&self` and `&mut self` for receiver methods.
- Mutable state is modeled with `var` fields or locals and ordinary assignment.
- Descriptor-backed standard types are used but not declared in this fixture.
- The fixture avoids user-defined generic declarations, traits, reflection,
  staging, closures, and full cosmo1 implementation logic.

## Exercised Forms

The corpus intentionally includes compiler-shaped declarations for source files,
spans, diagnostics, symbols, tokens, AST nodes, arenas, typed identifiers, scopes,
maps, sets, and parser state.

It also includes representative expression forms: class fields, enum-style cases,
methods, receiver references, mutation, field access, calls, assignments,
conditionals, `while`/`loop`, descriptor-backed variant construction, and `match`.

## Validation

Current repository validation is lightweight:

- The manifest lists the corpus path and phase intent.
- The existing shared parser parses the corpus.

Later cosmo0 work should consume this same file in order: subset elaboration,
typed checking, LIR lowering, backend emission, and package-level validation.

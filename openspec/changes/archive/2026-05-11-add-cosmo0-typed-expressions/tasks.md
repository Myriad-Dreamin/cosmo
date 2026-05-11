## 1. Type Model

- [x] 1.1 Define cosmo0 source-level types for unit, booleans, integers, `usize`, bytes, chars, strings, user declarations, aliases, references, and never/error placeholders.
- [x] 1.2 Define sealed standard generic type applications for descriptor-backed types such as `Vec`, `Option`, `Result`, `Arena`, `Id`, `Map`, `Set`, `Ptr`, and `Box`.
- [x] 1.3 Add alias expansion and type equality rules for the cosmo0 source type model.
- [x] 1.4 Add descriptor signature metadata needed for source-level method checking.

## 2. Typed Expression Representation

- [x] 2.1 Define typed modules, declarations, statements, expressions, patterns, and callable signatures.
- [x] 2.2 Attach resolved type information and source spans to each typed expression node.
- [x] 2.3 Represent mutable and immutable references explicitly in typed expression nodes.

## 3. Front-End Typer

- [x] 3.1 Add declaration collection for classes, fields, variants, methods, functions, aliases, and imports accepted by the untyped elaborator.
- [x] 3.2 Add scoped name resolution for parameters, locals, fields, functions, methods, aliases, and descriptor-defined names.
- [x] 3.3 Type-check literals, locals, assignments, field access, direct calls, method calls, returns, blocks, conditionals, loops, variant construction, and match expressions.
- [x] 3.4 Type-check source-level reference and mutability rules for `&T`, `&mut T`, `&self`, and `&mut self`.
- [x] 3.5 Type-check descriptor method surfaces for the early standard generics needed by compiler-shaped code.

## 4. Diagnostics And Tests

- [x] 4.1 Emit source diagnostics for unresolved names, wrong arity, invalid fields, invalid calls, assignment mismatch, return mismatch, invalid match payloads, and invalid mutability.
- [x] 4.2 Add positive tests that produce typed expressions for accepted source snippets.
- [x] 4.3 Add negative tests for representative type errors and invalid descriptor usage.
- [x] 4.4 Confirm the typed-expression typer does not call the existing full Cosmo `Typer`.

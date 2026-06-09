## 1. Provider Registry

- [x] 1.1 Add a gated compiler-hosted expression macro provider registry for
  smoke providers.
- [x] 1.2 Validate provider signatures as
  `@macro def provider(input: Expr[Untyped]): Expr[Untyped]`.
- [x] 1.3 Add deterministic diagnostics for missing, duplicate, disabled, and
  invalid provider entries.

## 2. Macro Classification

- [x] 2.1 Classify free call macro sites only when ordinary callee resolution
  selects a macro provider.
- [x] 2.2 Classify method-like macro sites only after receiver type facts make
  inherent member lookup runnable.
- [x] 2.3 Wait for method-set facts keyed by receiver type and selector name
  when trait or extension lookup can contribute method-like macro candidates.
- [x] 2.4 Classify block-attached sites only after target lookup selects a
  macro provider.
- [x] 2.5 Classify template/interpolation sites only after tag lookup selects a
  macro provider.
- [x] 2.6 Reject textual macro matching and unsupported payload shapes with
  stable diagnostics.

## 3. Payload Construction

- [x] 3.1 Build `Expr.Args` payloads for parenthesized argument syntax.
- [x] 3.2 Preserve receivers in `Expr.Args` for method-like syntax.
- [x] 3.3 Build `Expr.Block` payloads for accepted block-attached syntax.
- [x] 3.4 Build `Expr.Template` payloads for accepted template/interpolation
  syntax.
- [x] 3.5 Enforce the single-payload rule for forms such as `A(1) { block }`.

## 4. Provider Invocation

- [x] 4.1 Serialize macro function input with provider identity, source package
  identity, invocation identity, payload, spans, and origin metadata.
- [x] 4.2 Invoke compiler-hosted smoke providers through the same input/output
  protocol future providers must use.
- [x] 4.3 Validate provider diagnostics and generated `Expr[Untyped]` output.
- [x] 4.4 Assign stable generated origins before reintegrating output.

## 5. Rechecking And Diagnostics

- [x] 5.1 Splice generated expression output back into the untyped expression
  tree and run ordinary resolution/type checking.
- [x] 5.2 Reject typed-expression injection or direct compiler mutation attempts.
- [x] 5.3 Add generated-expression summaries for fixtures.
- [x] 5.4 Add coverage for disabled macro calls, unresolved providers, invalid
  output, expansion recursion, and deterministic ordering.
- [x] 5.5 Run the relevant cosmo0 tests and `scripts/check-scala-style.sh` if
  Scala sources are edited.

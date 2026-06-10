## Context

The active macro-system proposal defines the public expression macro shape:

```text
@macro def provider(input: Expr[Untyped]): Expr[Untyped]
```

It also defines that provider input is a normalized payload such as
`Expr.Args`, `Expr.Block`, or `Expr.Template`. This change implements a narrow
expression macro-call slice so the compiler can prove target selection,
payload construction, provider invocation, generated expression validation, and
ordinary rechecking.

## Goals / Non-Goals

**Goals:**

- Support compiler-hosted expression macro providers in a gated profile.
- Select free call, method-like, block-attached, and template macro sites
  through resolved targets, not by string name.
- Construct exactly one normalized payload for each accepted macro invocation.
- Invoke providers through the macro function input/output boundary.
- Validate provider output and splice generated `Expr[Untyped]` back into the
  untyped expression tree for ordinary checking.
- Emit deterministic diagnostics and expansion summaries for fixtures.

**Non-Goals:**

- Implement custom derive macros or declaration-generation workflows.
- Load self-hosted macro provider packages.
- Compile provider code through cosmo0 eval in this slice.
- Expose typed expression trees to macro providers.
- Support multiple provider payloads for one surface form such as
  `A(1) { block }`.
- Allow macro providers to mutate typed modules, lowering IR, backend output, or
  compiler global state.

## Decisions

### Use Resolved Targets For Macro Classification

Macro calls are classified only after name/member/tag resolution selects a macro
provider. A free `@macro def expand(...)` does not match every `.expand`
selector. Method-like macro calls wait for receiver facts before resolving the
selected member or extension.

Alternative considered: classify macros by syntax and callee text during
parsing. That would make `value.expand(2)` unsound and conflict with ordinary
member/extension lookup.

### Keep One Payload Per Invocation

Each accepted invocation passes exactly one normalized payload. Parenthesized
argument syntax and method-like syntax become `Expr.Args`; attached blocks
become `Expr.Block`; templates and interpolations become `Expr.Template`.

Alternative considered: expose multiple provider parameters matching surface
syntax. That would make the macro ABI depend on parser shapes and conflict with
the current single-payload boundary.

### Start With Compiler-Hosted Providers

The first macro-call implementation should use registered compiler-hosted smoke
providers. They still receive serialized macro input and return serialized
macro output, but they avoid blocking this slice on self-hosted provider
package compilation.

Alternative considered: require CTE-backed self-hosted providers first. That
would merge expression call implementation with macro package graph, provider
ABI generation, and native execution work.

### Recheck Generated Expressions

Provider output is an untrusted `Expr[Untyped]`. The compiler validates the
shape, assigns origin metadata, splices it into the untyped tree, and then runs
ordinary resolution and type checking. The provider cannot return a trusted
`TypedExpr`.

Alternative considered: let providers return typed artifacts. That would bypass
ordinary type checking and make macro output a type-system escape hatch.

### Keep Expansion Deterministic

Runnable macro invocations may be evaluated in parallel, but diagnostics,
generated expression summaries, and reintegration order are sorted by stable
invocation identity and source span.

Alternative considered: preserve provider execution order as source semantics.
That would conflict with the macro purity contract and limit parallel
expansion.

## Risks / Trade-offs

- Macro calls can obscure generated code -> emit generated-expression summaries
  for fixtures and attach origin spans.
- Compiler-hosted providers can become permanent special cases -> require the
  same input/output contract that future self-hosted providers must satisfy.
- Method-like macro calls need receiver facts -> model unresolved selectors as
  delayed obligations instead of forcing source-order checking.
- Generated output can recursively introduce macros -> reject or bound recursive
  expansion in this slice.

## Migration Plan

1. Land the macro-system documentation and compile-time evaluation boundary.
2. Add compiler-hosted smoke providers and deterministic provider registry
   diagnostics.
3. Implement resolved-target macro classification for accepted call sites.
4. Add payload construction for `Expr.Args`, `Expr.Block`, and `Expr.Template`.
5. Invoke providers, validate generated expressions, and recheck output.
6. Leave self-hosted provider packages and CTE-backed provider execution for a
   later change.

## Open Questions

- Which smoke providers should be kept as long-term fixtures versus removed
  once self-hosted providers exist.
- Whether recursive macro expansion should be rejected outright in the first
  implementation or allowed with a small deterministic expansion depth.

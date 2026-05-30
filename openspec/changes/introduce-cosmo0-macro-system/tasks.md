## 1. Documentation And Specs

- [ ] 1.1 Add `docs/cosmo0/macro-expr.typ` covering `Expr[T = Untyped]`, expression macros, typed inspectors, and non-goals.
- [ ] 1.2 Add `docs/cosmo0/compile-time-evaluation.typ` covering `ConstEval`, `ProviderEval`, purity, capabilities, C++ JIT support through `cosmo-jit-sys`, and runtime separation.
- [ ] 1.3 Link macro documentation from `docs/cosmo0/spec.typ`, include it in the website book summary, and update the docs validation script.
- [ ] 1.4 Add examples for `@derive(...)`, declaration attributes, expression macros, generated output, and unsupported macro shapes.
- [ ] 1.5 Document the first compiler-hosted provider boundary and the future self-hosted provider path.

## 2. Syntax And Attribute Preservation

- [ ] 2.1 Extend parsed/elaborated declaration models to preserve structured macro attributes on supported declaration shapes.
- [ ] 2.2 Keep existing `@include` and `@extern` behavior unchanged.
- [ ] 2.3 Add diagnostics for malformed macro attributes, repeated keyed arguments, and attributes attached to unsupported syntax nodes.
- [ ] 2.4 Add fixture coverage proving non-macro profiles still reject unsupported decorators.

## 3. Reflection Metadata

- [ ] 3.1 Add reflection metadata records for classes, fields, functions, variants, defaults, docs, attributes, visibility, and spans.
- [ ] 3.2 Populate reflection metadata from parsed and resolved declaration shapes.
- [ ] 3.3 Add deterministic display/serialization for reflection metadata to support golden tests.
- [ ] 3.4 Add diagnostics for metadata that cannot be produced because a declaration shape is unsupported.

## 4. Compile-Time Expression And Value Model

- [ ] 4.1 Add distinct data models for `AttrExpr`, `ConstValue`, `Expr[T = Untyped]` as untyped source expression, and generated declaration trees.
- [ ] 4.2 Restrict first-slice attribute expressions to literals, paths, type references, arrays, records, and keyed arguments.
- [ ] 4.3 Add diagnostics for unsupported compile-time expression forms.
- [ ] 4.4 Add deterministic display/serialization for compile-time values and generated declaration trees.
- [ ] 4.5 Add typer-phase inspectors for typed expression facts, such as `Type.of(expr)`, without exposing `TypedExpr` trees to providers.

## 5. Macro Expansion Engine

- [ ] 5.1 Add a package pipeline phase that runs after declaration-shape collection and before body checking/lowering.
- [ ] 5.2 Add a macro provider registry keyed by resolved derive/provider paths.
- [ ] 5.3 Define the provider input/output contract: reflection metadata and const values in, generated declaration trees and diagnostics out.
- [ ] 5.4 Integrate generated declarations into name resolution and type checking.
- [ ] 5.5 Add generated-span plumbing for diagnostics and generated-source summaries.

## 6. Compile-Time Evaluation Boundary

- [ ] 6.1 Add a compile-time evaluator interface with explicit input, output, diagnostics, and capability set.
- [ ] 6.2 Route compiler-hosted providers through the same boundary used by future self-hosted providers.
- [ ] 6.3 Add diagnostics for side effects, runtime-only APIs, and unsupported compile-time calls.
- [ ] 6.4 Add budget/fuel controls for the future self-hosted interpreter path.
- [ ] 6.5 Document and enforce the macro-function purity contract where feasible; non-pure providers have undefined behavior.
- [ ] 6.6 Route admitted C++ JIT support through `cosmo-jit-sys` with declared inputs, opaque symbol tokens, structured diagnostics, and deterministic capability checks.

## 7. Hygiene And Validation

- [ ] 7.1 Implement fresh internal symbol generation for macro helpers.
- [ ] 7.2 Diagnose collisions between public generated declarations and user declarations.
- [ ] 7.3 Reject unconsumed macro attributes after expansion.
- [ ] 7.4 Add deterministic repeated-expansion tests.

## 8. Derive Provider Smoke Path

- [ ] 8.1 Implement a minimal compiler-hosted derive provider that generates a simple static method from reflected fields.
- [ ] 8.2 Add positive tests proving user code can call the generated method.
- [ ] 8.3 Add negative tests for unresolved derive provider paths and unsupported target types.
- [ ] 8.4 Add end-to-end tests that keep generated code in ordinary checking and C++ backend paths.

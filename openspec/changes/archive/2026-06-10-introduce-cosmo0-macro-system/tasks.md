## 1. Documentation And Specs

- [x] 1.1 Add `docs/cosmo/macro-expr.typ` covering `Expr[T = Untyped]`, expression macros, typed inspectors, and non-goals.
- [x] 1.2 Add `docs/cosmo/compile-time-evaluation.typ` covering macro function input/output records, macro output purity, full C++ compile-time execution through cosmo0 eval, and target runtime separation.
- [x] 1.3 Link macro documentation from `docs/cosmo/spec.typ`, include it in the website book summary, and update the docs validation script.
- [x] 1.4 Add examples for `@derive(...)`, declaration attributes, expression macros, generated output, and unsupported macro shapes.
- [x] 1.5 Document that first-slice derive output attaches trait implementations
  without adding new ordinary name-resolution bindings.
- [x] 1.6 Document the first compiler-hosted provider boundary and the future self-hosted provider path.

## 2. Syntax And Attribute Preservation

- [x] 2.1 Extend parsed/elaborated declaration models to preserve structured macro attributes on supported declaration shapes.
- [x] 2.2 Keep existing `@include` and `@extern` behavior unchanged.
- [x] 2.3 Add diagnostics for malformed macro attributes, repeated keyed arguments, and attributes attached to unsupported syntax nodes.
- [x] 2.4 Add fixture coverage proving non-macro profiles still reject unsupported decorators.

## 3. Reflection Metadata

- [ ] 3.1 Add reflection metadata records for classes, fields, functions, variants, defaults, docs, attributes, visibility, and spans.
- [ ] 3.2 Populate reflection metadata from parsed and resolved declaration shapes.
- [x] 3.3 Add deterministic display/serialization for reflection metadata to support golden tests.
- [x] 3.4 Add diagnostics for metadata that cannot be produced because a declaration shape is unsupported.

## 4. Macro Input And Expression Model

- [x] 4.1 Add distinct data models for `Expr[T = Untyped]` as untyped source expression, serialized macro function input, serialized macro function output, and generated declaration trees.
- [x] 4.2 Define the admitted attribute/default payloads that may appear in serialized macro function input.
- [x] 4.3 Add diagnostics for unsupported macro input payloads.
- [x] 4.4 Add deterministic display/serialization for macro function input/output records and generated declaration trees.
- [ ] 4.5 Add typer-phase inspectors for typed expression facts, such as `Type.of(expr)`, without exposing `TypedExpr` trees to providers.

## 5. Macro Expansion Engine

- [x] 5.1 Add an expression-checker expansion hook that expands macro calls before ordinary call checking.
- [x] 5.2 Add a compiler-hosted expression macro provider registry keyed by provider paths.
- [x] 5.3 Define the provider input/output contract: serialized macro function input in, generated artifacts and diagnostics out.
- [x] 5.4 Integrate expression macro output by recursively checking returned `Expr[Untyped]` in the caller scope and expected type.
- [x] 5.5 Add generated-span plumbing for diagnostics and generated-source summaries.

## 6. Compile-Time Evaluation Boundary

- [ ] 6.1 Add a compile-time evaluator interface with serialized macro function input, serialized macro function output, diagnostics, and C++ provider-entry compile context.
- [x] 6.2 Route compiler-hosted providers through the same boundary used by future self-hosted providers.
- [ ] 6.3 Add diagnostics where feasible for provider behavior that cannot preserve deterministic macro output.
- [ ] 6.4 Add resource limits for provider-entry compilation, provider execution, generated artifacts, and repeated evaluation.
- [ ] 6.5 Document and enforce the macro-function purity contract where feasible; non-pure providers have undefined behavior.
- [ ] 6.6 Route C++ compile-time execution through cosmo0 eval with provider-entry compilation, PCH/precompiled context reuse, imported C++ types, provider code execution, structured diagnostics, and Clang-owned layout facts.
- [ ] 6.7 Reject clangInterpreter, JS host JIT, or handwritten C++ layout emulation as the source of C++ struct/class/template facts.

## 7. Hygiene And Validation

- [ ] 7.1 Implement fresh internal symbol generation for macro helpers.
- [ ] 7.2 Diagnose collisions between public generated declarations and user declarations.
- [ ] 7.3 Reject unconsumed macro attributes after declaration/derive macro expansion.
- [x] 7.4 Add deterministic repeated-expansion tests.

## 8. Expression Provider Smoke Path

- [x] 8.1 Implement minimal compiler-hosted expression providers for answer, identity, and invalid-output smoke coverage.
- [x] 8.2 Add positive tests proving expression macro output is checked in the caller's expected type.
- [x] 8.3 Add negative tests for unresolved expression provider paths, invalid provider output, unsupported input, and unsupported profiles.
- [x] 8.4 Add end-to-end tests that keep generated expression output in ordinary checking and C++ backend paths.

## Context

The prior cosmo0 proposal defines a Scala-implemented core compiler that accepts a restricted Cosmo subset with sealed standard generics. That proposal deliberately avoids full self-hosting and avoids making cosmo0 a second full implementation of Cosmo.

cosmo1 is the next architectural target: the full Cosmo compiler written in cosmo0-compatible source. cosmo1 must be able to implement full Cosmo features such as type-level values, user generics, trait resolution, reflection, and staging. Those features should appear as cosmo1 compiler data structures and algorithms, not as cosmo0 host-language features.

The immediate design problem is to list what cosmo1 needs so cosmo0 can be built to support the compiler without becoming full Cosmo prematurely.

## Goals / Non-Goals

**Goals:**

- Define a concrete cosmo1 source tree and module boundary.
- Identify every major compiler subsystem that should become a cosmo1 file or file group.
- Identify core0 standard types required to express cosmo1 in cosmo0.
- Identify cosmo1-owned data types for syntax, diagnostics, resolution, typing, evaluation, and code generation.
- Define compiler capabilities cosmo1 must implement and the smaller cosmo0 capabilities needed to write them.
- Provide staged bootstrap targets that can be verified independently.

**Non-Goals:**

- Implement cosmo1.
- Require cosmo0 to support user-defined generics, trait solving, type-level computation, reflection, staging, or macros.
- Require cosmo1 to match the current Scala compiler behavior in the first stage.
- Remove the current Scala full compiler before cosmo1 reaches parity.

## Decisions

### Place cosmo1 in a dedicated package

cosmo1 source should live in a dedicated package, tentatively:

```text
packages/cosmo1/
  cosmo.json
  src/
```

The package name should be distinct from the current Scala compiler and existing `packages/cosmoc` experiments. A reasonable package identity is `@cosmo/cosmo1` or `@cosmo/compiler1`; the final name can be chosen before implementation.

Alternative considered: continue using `packages/cosmoc/src`. That keeps existing experiments nearby, but it blurs bootstrap-target code with exploratory code and makes acceptance harder to define.

### Use layered compiler modules

cosmo1 should be organized as a traditional compiler pipeline. Proposed files:

```text
packages/cosmo1/src/main.cos

packages/cosmo1/src/driver/config.cos
packages/cosmo1/src/driver/session.cos
packages/cosmo1/src/driver/compile.cos
packages/cosmo1/src/driver/diagnostic.cos

packages/cosmo1/src/source/source.cos
packages/cosmo1/src/source/span.cos
packages/cosmo1/src/source/source_map.cos

packages/cosmo1/src/lex/token.cos
packages/cosmo1/src/lex/lexer.cos

packages/cosmo1/src/syntax/ast.cos
packages/cosmo1/src/syntax/parser.cos
packages/cosmo1/src/syntax/json_loader.cos
packages/cosmo1/src/syntax/pretty.cos

packages/cosmo1/src/package/meta.cos
packages/cosmo1/src/package/loader.cos
packages/cosmo1/src/package/module_graph.cos

packages/cosmo1/src/names/symbol.cos
packages/cosmo1/src/names/scope.cos
packages/cosmo1/src/names/resolve.cos

packages/cosmo1/src/types/type_expr.cos
packages/cosmo1/src/types/ty.cos
packages/cosmo1/src/types/env.cos
packages/cosmo1/src/types/check.cos
packages/cosmo1/src/types/subtype.cos
packages/cosmo1/src/types/normalize.cos

packages/cosmo1/src/eval/value.cos
packages/cosmo1/src/eval/interpreter.cos
packages/cosmo1/src/eval/builtins.cos

packages/cosmo1/src/ir/hir.cos
packages/cosmo1/src/ir/tir.cos
packages/cosmo1/src/ir/lower.cos

packages/cosmo1/src/codegen/cpp/ast.cos
packages/cosmo1/src/codegen/cpp/emit.cos
packages/cosmo1/src/codegen/cpp/names.cos
packages/cosmo1/src/codegen/cpp/runtime.cos

packages/cosmo1/src/link/artifact.cos
packages/cosmo1/src/link/cmake.cos
packages/cosmo1/src/link/command.cos

packages/cosmo1/src/cache/depfile.cos
packages/cosmo1/src/cache/ir_cache.cos
packages/cosmo1/src/cache/scope_json.cos

packages/cosmo1/src/tests/smoke.cos
```

The `json_loader.cos` file is transitional. It allows cosmo1 to consume AST JSON from the existing Scala parser while `syntax/parser.cos` matures.

Alternative considered: begin with only parser/codegen files. That is faster initially but does not expose the actual standard type pressure from resolution, typing, diagnostics, artifacts, and driver behavior.

### Model full Cosmo features as compiler data

cosmo1 should represent full Cosmo language concepts as data:

```text
Ty.Universe
Ty.App
Ty.TypeFunction
Ty.Trait
Ty.Impl
CtValue.Type
CtValue.Expr
Quote
MacroInput
```

This does not imply cosmo0 supports these as host-language features. cosmo0 only needs classes, variants, matching, methods, sealed standard generics, references, mutation, and ordinary control flow.

Alternative considered: add enough type-level computation to cosmo0 to write the type checker more naturally. That recreates the bootstrap cycle and makes cosmo0 hard to stabilize.

### Prefer arena IDs over pervasive raw pointers

cosmo1 should use `Arena<T>` and `Id<T>` for AST, type, scope, definition, and IR storage:

```text
ExprId = Id<Expr>
TyId = Id<Ty>
DefId = Id<DefInfo>
ScopeId = Id<Scope>
```

Raw `Ptr<T>` remains necessary for interop and low-level runtime edges, but compiler internals should prefer stable arena IDs. This makes ownership, serialization, and diagnostics easier to reason about.

Alternative considered: use `Ptr<T>` throughout the compiler. That is closer to C++ but makes lifetime and mutation boundaries less explicit.

### Use deterministic collections by default

Compiler behavior must be stable for snapshots, generated code, and diagnostics. `Map<K, V>` should be deterministic by default, or cosmo1 should use an explicit `OrderedMap<K, V>` for all iteration-sensitive state.

Alternative considered: use hash maps everywhere. That is faster in some paths but risks nondeterministic output unless every iteration is sorted.

### Stage capability growth

cosmo1 should not aim for full parity in one step. The first useful targets are:

1. Source loading, spans, diagnostics, tokens, and a small parser.
2. AST arena and JSON loader parity with the current parser JSON shape.
3. Name resolution and package/module graph for a small package.
4. Type representation and a narrow type checker.
5. Compile-time value representation and minimal evaluator.
6. C++ emission for a small accepted program.

Each stage should be compilable through cosmo0.

## Risks / Trade-offs

- cosmo1 file layout may over-specify before implementation -> Mitigation: mark names as proposed and allow file grouping changes if capability boundaries remain stable.
- Standard type list may force cosmo0 runtime work early -> Mitigation: classify types into required tiers and implement only the stage-required subset.
- `Arena<T>` and `Id<T>` require phantom generic behavior -> Mitigation: keep them sealed standard generics and do not permit user-defined phantom generics.
- Full Cosmo type checker may pressure cosmo0 into supporting host generics -> Mitigation: model type variables and generic declarations as ordinary `Ty` and AST data.
- Deterministic collections may be slower -> Mitigation: prioritize stable compiler output first; add explicit non-deterministic collections only for internal paths that never affect output order.

## Migration Plan

1. Accept this architecture proposal as the cosmo1 target.
2. Extend the cosmo0/core0 descriptor work with the standard types required by the first cosmo1 stage.
3. Create the dedicated cosmo1 package skeleton.
4. Implement source, span, diagnostic, token, and lexer/parser smoke modules first.
5. Add JSON AST loading to bridge from the existing Scala parser while native cosmo1 parsing matures.
6. Grow name resolution, typing, eval, and codegen in staged milestones.
7. Keep the Scala full compiler available until cosmo1 reaches explicit parity checkpoints.

Rollback is additive: remove the cosmo1 package or exclude it from build/test targets while leaving cosmo0 and the existing Scala compiler intact.

## Open Questions

- Should the package be named `@cosmo/cosmo1`, `@cosmo/compiler1`, or reuse `@cosmo/compiler` after migration?
- Should deterministic `Map<K, V>` be the default, or should the standard library expose both `Map` and `HashMap` from the start?
- Should `Span`, `Diagnostic`, `Symbol`, and `Arena<T>` live in core0 std or in cosmo1-owned modules?
- How much of the current Scala parser JSON format should become a stable bridge format?
- Which current sample should be the first end-to-end cosmo1 acceptance program?

## Context

cosmo0 currently accepts only a small set of meaningful decorators:
file-level `@include(...)` and trusted `@extern(...)` metadata. Other decorators
and staging annotations are diagnosed as unsupported, even though full Cosmo
syntax already reserves decorator space. That keeps bootstrap behavior small,
but it blocks practical libraries whose ergonomics depend on generated code.

The derived CLI library is the forcing function for this change. A usable
`@derive(cli.Parser)` needs declaration metadata, field attributes, defaults,
sum-type variants, deterministic generated code, and diagnostics that point back
to the user's struct. Those same primitives are also useful for future serde,
schema, RPC, and test-discovery libraries.

## Goals / Non-Goals

**Goals:**

- Add a macro expansion phase that can generate ordinary cosmo0 declarations.
- Support custom derive macros on classes and sum types.
- Preserve macro-owned attributes on declarations, fields, variants, and
  functions long enough for macro providers to inspect them.
- Provide typed reflection metadata for derive providers.
- Keep generated output deterministic and reviewable.
- Give generated declarations stable names and diagnostics with source spans.
- Allow a compiler-hosted macro provider implementation as the first bridge.
- Keep the macro contract compatible with later self-hosted macro packages.
- Define how macro providers are computed instead of leaving provider execution
  to ad hoc compiler callbacks.
- Separate source expressions, attribute expressions, compile-time values, and
  generated expressions into explicit data models.
- Define the first compile-time evaluator/interpreter boundary without running
  target program code.

**Non-Goals:**

- Add arbitrary expression quotation or function-like expression macros in the
  first slice.
- Allow macros to perform filesystem, command, network, or runtime effects.
- Require all macro providers to be written in Cosmo before the macro execution
  host exists.
- Make runtime reflection a dependency of generated parsers.
- Admit every existing full-language staging feature into cosmo0.
- Replace ordinary type checking with macro-time execution.
- Interpret arbitrary user functions at compile time in the first slice.
- Allow macro providers to inspect arbitrary function bodies unless a later
  capability admits quoted expression metadata.

## Decisions

### Add A Dedicated Expansion Stage

Macro expansion should run after parsing and declaration-shape collection, but
before ordinary body checking and LIR lowering. Derive providers need enough
shape information to know a class name, field list, field types, variants, and
attributes. They should not need fully checked function bodies.

Alternative considered: run macros directly during parsing. That cannot support
type-aware derives such as CLI value parsing or future serialization derives.

Alternative considered: run macros after complete type checking. That makes
generated methods unavailable to name resolution and type checking of user code.

### Treat Derived Code As Ordinary Generated Declarations

A macro provider should return generated declarations plus diagnostics, not a
private backend hook. The generated declarations then enter the same
resolution, checking, lowering, and backend paths as hand-written declarations.

Alternative considered: special-case each derive in the compiler. That would
ship features quickly but would make every library derive a compiler feature.

### Preserve Attributes As Data

The elaborator should preserve macro-owned attributes as structured attribute
data. Known compiler decorators such as `@include` and `@extern` keep their
existing behavior. Unknown attributes are accepted only when they are attached to
a declaration shape that a macro provider can consume or when the profile admits
the attribute namespace.

Alternative considered: store attributes as raw source text. That would make
macro providers re-parse syntax and would produce weaker diagnostics.

### Use Conservative Hygiene

Generated helper names should be fresh by default and should not shadow user
names silently. When a macro intentionally exposes a generated member such as
`Cli::parse`, the provider must declare that public output and conflict with
user declarations deterministically.

Alternative considered: let generated text behave exactly like pasted source.
That is easy to implement but quickly creates fragile name-capture behavior.

### Start With Compiler-Hosted Providers

The first implementation may register macro providers in Scala-side cosmo0
infrastructure. The provider contract should still be specified in source terms:
metadata in, generated declarations and diagnostics out. This lets the CLI
derive prove the macro system before self-hosted macro packages exist.

Alternative considered: require self-hosted Cosmo macro execution first. That
would make the CLI library depend on a larger compiler bootstrapping problem.

### Define Macro Computation As A Provider Evaluation Protocol

Macro computation should not mean "execute arbitrary Cosmo code during
compilation". A macro invocation is evaluated through a provider protocol:

```text
MacroInput:
  target declaration metadata
  consumed attribute candidates
  admitted compile-time constants
  provider configuration

MacroOutput:
  generated declarations
  consumed attributes
  diagnostics
  optional generated-source summary
```

The first provider execution host can be Scala-side compiler infrastructure.
That host is still constrained by the same input/output protocol and capability
boundary as future self-hosted providers. It must not read undeclared files,
spawn commands, inspect ambient environment variables, perform network IO, or
execute target runtime code.

When self-hosted providers arrive, they should run inside a dedicated
compile-time evaluator over a restricted macro IR. That evaluator is part of the
compiler, not the generated program and not the target C++ backend. Its inputs
are explicit macro metadata and declared compile-time resources.

Alternative considered: treat macro providers as ordinary functions and call
them with the host runtime. That makes expansion depend on runtime state,
backend availability, and side effects.

Alternative considered: only allow hard-coded compiler providers forever. That
keeps execution controlled but prevents user-defined libraries from owning their
derive behavior.

### Separate Expression And Value Kinds

The macro system should not expose one universal `Expr` type that means parsed
source, attribute syntax, compile-time value, generated code, and typed
expression depending on context. The first model should keep these kinds
separate:

```text
SourceExpr:
  parsed user expression with spans; not generally exposed to derive macros in
  the first slice

AttrExpr:
  restricted attribute argument syntax, initially literals, paths, type
  references, arrays, records, and keyed arguments admitted by the provider

ConstValue:
  evaluated compile-time value such as Bool, String, integer text/value,
  TypeRef, PathRef, array, record, or enum-like tag

GeneratedExpr:
  expression node inside generated declarations, produced through a declaration
  builder API and checked later by the ordinary type checker

TypedExpr:
  ordinary checked expression artifact after macro expansion; not constructed
  directly by macro providers in the first slice
```

Derive macros primarily consume reflection metadata plus `AttrExpr` values
lowered to `ConstValue`. They produce `GeneratedDecl` trees that may contain
`GeneratedExpr` nodes. Generated expressions are type checked later with normal
source code; the provider does not get to manufacture trusted typed
expressions.

Alternative considered: let providers return raw source strings. That is easy
to prototype but makes hygiene, diagnostics, formatting, and validation
unreliable.

Alternative considered: let providers return fully typed expressions. That
turns macros into a type-checker escape hatch.

### Use A Restricted Compile-Time Evaluator

Compile-time evaluation should be a separately specified compiler service used
by macro expansion and, later, const evaluation. It should evaluate only pure,
terminating, deterministic macro IR. The first accepted values are enough for
attributes and derive configuration: booleans, strings, integer literals or
checked integers, type references, symbol paths, arrays, records, and simple
enum-like tags.

The evaluator should not interpret arbitrary user code in the first slice.
Provider code can be compiler-hosted first; self-hosted providers later compile
to macro IR that the evaluator can run with a fuel/step budget and an explicit
capability set.

Alternative considered: use the runtime interpreter or generated executable for
compile-time computation. That confuses compile-time and runtime semantics and
would make macro expansion depend on backend execution.

Alternative considered: no evaluator, only raw attribute syntax. That works for
the CLI MVP but prevents typed defaults, reusable constants, and future
self-hosted derive providers.

### Keep Reflection Compile-Time Only

The reflection data in this change exists for macro expansion. It is not a
runtime reflection API and does not require programs to carry type metadata at
runtime.

Alternative considered: expose one reflection system for compile time and
runtime immediately. That would expand ABI, backend, and optimization concerns
before the macro use case needs them.

## Risks / Trade-offs

- Macro expansion can make compilation harder to debug -> emit deterministic
  generated-source summaries and keep generated spans linked to macro inputs.
- Compiler-hosted providers can become permanent compiler magic -> require the
  same provider contract that later self-hosted providers must satisfy.
- Attribute acceptance can hide typos -> reject unconsumed attributes after
  derive expansion unless a provider explicitly accepts them.
- Derive macros can produce unstable helper names -> require stable naming rules
  and duplicate-name diagnostics.
- Reflection metadata can grow without control -> scope the first metadata set
  to declaration shapes needed by derives.
- Macro execution can accidentally become an unrestricted interpreter -> keep a
  separate compile-time evaluator spec with explicit value types, capability
  checks, and fuel limits.
- A single loose Expr model can leak implementation details across phases ->
  keep `SourceExpr`, `AttrExpr`, `ConstValue`, `GeneratedExpr`, and `TypedExpr`
  distinct in APIs and tests.

## Migration Plan

1. Preserve macro attributes in the parser/elaborator while keeping old
   unsupported-decorator diagnostics for non-macro profiles.
2. Add macro expansion artifacts and diagnostics behind a profile or package
   capability gate.
3. Implement compiler-hosted derive provider registration.
4. Add the restricted attribute expression and compile-time value model.
5. Add generated declaration builders and generated-source debug output.
6. Add deterministic tests for provider execution and evaluator boundaries.
7. Enable the CLI derive provider as the first end-to-end consumer.

## Open Questions

- Whether macro provider packages should be declared through `macroDependencies`
  in `cosmo.json` or through ordinary dependencies plus provider metadata.
- How much of the reflection metadata should be available to ordinary
  compile-time code once self-hosted macros exist.
- Which textual syntax should be used for debugging generated declaration trees
  once the builder API becomes the primary output format.

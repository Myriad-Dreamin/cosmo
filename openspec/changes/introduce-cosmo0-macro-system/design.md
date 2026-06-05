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

- Add a macro expansion phase that can generate validated cosmo0 artifacts.
- Support custom derive macros on classes and sum types.
- Preserve macro-owned attributes on declarations, fields, variants, and
  functions long enough for macro providers to inspect them.
- Provide typed reflection metadata for derive providers.
- Keep generated output deterministic and reviewable.
- Give generated artifacts stable origins and diagnostics with source spans.
- Allow a compiler-hosted macro provider implementation as the first bridge.
- Keep the macro contract compatible with later self-hosted macro packages.
- Define how macro providers are computed instead of leaving provider execution
  to ad hoc compiler callbacks.
- Define `Expr[T = Untyped]` as the macro API's untyped source-expression value
  and keep serialized macro function input/output records separate from it.
- Define typed-expression information as inspector output from the typer phase,
  not as a typed expression tree that macro providers can receive or construct.
- Define compile-time macro function execution through `cosmo-cte-sys`
  provider-entry compilation with PCH/precompiled context reuse, without using
  clangInterpreter or approximating C++ execution in JavaScript.

**Non-Goals:**

- Add arbitrary expression quotation or function-like expression macros in the
  first slice.
- Let first-slice derive macros add new top-level declarations, members, fields,
  variants, classes, aliases, or ordinary name-resolution bindings.
- Define macro semantics where output depends on ambient filesystem, command,
  network, time, randomness, or runtime effects.
- Require all macro providers to be written in Cosmo before the macro execution
  host exists.
- Make runtime reflection a dependency of generated parsers.
- Admit every existing full-language staging feature into cosmo0.
- Replace ordinary type checking with macro-time execution.
- Treat clangInterpreter, JavaScript host JIT, or handwritten C++ layout tables
  as the source of C++ struct, class, template, or ABI-visible type facts.
- Allow macro providers to inspect arbitrary function bodies unless a later
  capability admits quoted expression metadata.
- Add a multi-stage `Expr[T]` or typed quotation system.
- Allow macro providers to receive, construct, or trust `TypedExpr` artifacts.

## Decisions

### Add A Dedicated Expansion Stage

Macro expansion should run after parsing and declaration-shape collection, but
before ordinary body checking and LIR lowering. Derive providers need enough
shape information to know a class name, field list, field types, variants, and
attributes. They should not need fully checked function bodies.

Alternative considered: run macros directly during parsing. That cannot support
type-aware derives such as CLI value parsing or future serialization derives.

Alternative considered: run all macros after complete type checking. That makes
generated artifacts unavailable to later checking. First-slice derive output is
more constrained: it attaches trait implementations without adding new names,
so it can run after declaration indexing without forcing ordinary name
resolution to be rebuilt.

### Treat Generated Output As Structured Artifacts

A macro provider should return structured generated artifacts plus diagnostics,
not a private backend hook. General declaration macros may return generated
declarations in a later slice. First-slice derive providers return trait
implementation attachments that enter ordinary impl validation without changing
the declaration index.

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
names silently. First-slice derive macros avoid this issue by not exposing new
members or top-level declarations at all. Later declaration macros that expose
public generated names must declare that output and conflict with user
declarations deterministically.

Alternative considered: let generated text behave exactly like pasted source.
That is easy to implement but quickly creates fragile name-capture behavior.

### Start With Compiler-Hosted Providers

The first implementation may register macro providers in Scala-side cosmo0
infrastructure. The provider contract should still be specified in source terms:
metadata in, generated artifacts and diagnostics out. This lets the CLI derive
prove the macro system before self-hosted macro packages exist.

Alternative considered: require self-hosted Cosmo macro execution first. That
would make the CLI library depend on a larger compiler bootstrapping problem.

### Define Macro Computation As A Serialized Function Protocol

Macro computation should not mean direct compiler mutation during compilation. A
macro invocation is evaluated through a serialized function protocol:

```text
MacroFunctionInput:
  provider identity
  source package identity
  macro call or macro target identity
  compiler-selected reflection facts
  admitted attributes and defaults
  expression fragments when applicable
  source spans and hygiene/origin metadata
  C++ import and execution context

MacroFunctionOutput:
  generated declarations
  consumed attributes
  diagnostics
  generated-source summary
  native support binding metadata when needed
```

The first provider execution host can be Scala-side compiler infrastructure.
That host is still constrained by the same serialized input/output protocol as
future self-hosted providers. If provider execution needs C++ type facts or C++
code execution, it should route through `cosmo-cte-sys` rather than using
clangInterpreter, approximating C++ in JavaScript, or mutating compiler
internals directly.

Every macro function is required to be pure with respect to the macro input
provided by cosmo0. For the same source package, provider identity, admitted
metadata, expression fragments, C++ imports, provider source, target settings,
and toolchain identity, repeated evaluation must return the same output and
diagnostics. The compiler may cache, discard, rerun, or compare macro
evaluations. If a macro function depends on hidden mutable state or ambient
effects and produces different results for the same cosmo0 input, the package
has undefined behavior.

Macro functions that need C++ capability should run through `cosmo-cte-sys`,
which compiles explicit provider entry functions as ordinary Clang code against
a declared C++ execution context. Heavy headers and support code should be
accelerated with PCH, Clang modules, module caches, or an equivalent
precompiled context. This gives providers Clang's C++ type system, template
instantiation, layout, alignment, padding, overload resolution, and execution
semantics during compilation while still returning serialized macro function
output to the compiler.

Alternative considered: treat macro providers as ordinary JavaScript host
functions, run them through a JS host JIT, or use clangInterpreter as the
semantic execution substrate. The JS paths approximate C++ struct/class layout,
padding, alignment, template instantiation, and ABI-visible type facts. The
clangInterpreter path can diverge from ordinary Clang compilation and may
miscompile provider code. Both are rejected as macro semantics.

Alternative considered: only allow hard-coded compiler providers forever. That
keeps execution controlled but prevents user-defined libraries from owning their
derive behavior.

### Use `Expr[T = Untyped]` As Untyped Source Expression

The macro system should not expose one universal compiler-internal `Expr` type
that sometimes means attribute syntax, sometimes means a compile-time value, and
sometimes means a trusted typed expression. The public macro expression value is
instead narrow:

```text
Expr[T = Untyped]:
  untyped source expression value with spans and hygiene/origin metadata;
  default T = Untyped is a macro-level phase marker, not an object-language
  runtime type, and it does not make the expression typed

MacroFunctionInput:
  serialized compiler-selected input facts, admitted attributes/defaults,
  expression fragments, source spans, hygiene/origin metadata, and C++ execution
  context

MacroFunctionOutput:
  serialized generated declarations, generated expression fragments,
  diagnostics, generated-source summary, and native support binding metadata
```

In the current macro contract, `Untyped` is the only stable expression type
argument. It is a distinguished marker for "not yet checked by the ordinary
typer"; providers must not interpret `T` as an arbitrary object-language type
parameter.

Derive macros primarily consume serialized macro function input selected by the
compiler. Expression macros consume and produce `Expr[Untyped]`. Generated
declarations may contain these expression fragments. All generated expressions
are type checked later with normal source code; the provider does not get to
manufacture trusted typed expressions.

Typed-expression information is exposed only through explicit typer-phase
inspectors over an `Expr` value, for example a `Type.of(expr)`-style query that
returns a stable type description for the corresponding expression after or
during ordinary typing. Such inspectors return facts, not a mutable `TypedExpr`
tree, and they do not authorize the provider to bypass rechecking.

Alternative considered: let providers return raw source strings. That is easy
to prototype but makes hygiene, diagnostics, formatting, and validation
unreliable.

Alternative considered: let providers return fully typed expressions. That
turns macros into a type-checker escape hatch.

### Use `cosmo-cte-sys` For C++ Compile-Time Execution

Compile-time macro function execution should be a separately specified compiler
boundary backed by `cosmo-cte-sys` when the provider needs C++ semantics. The
adapter receives serialized macro function input plus C++ imports, headers,
include/library context, provider source or generated entry function snippets,
target settings, compile options, precompiled context key, and toolchain
identity. It returns diagnostics and serialized macro function output.

The adapter may compile and execute C++ provider code and inspect imported C++
types, but it does not return raw compiler mutation handles. Generated
declarations and expression fragments still re-enter ordinary validation and
type checking.

Alternative considered: use a restricted interpreter over a small macro IR as
the semantic model. That was attractive for limited attribute evaluation, but it
does not provide full C++ type import, template instantiation, layout, padding,
alignment, overload resolution, or C++ code execution.

Alternative considered: use clangInterpreter or clang-repl. That preserves an
interactive model, but the interpreter path can diverge from ordinary Clang
compilation and is not accepted as the semantic substrate for macro output.

Alternative considered: use the generated target executable for compile-time
computation. That confuses provider-host execution with target package runtime
execution and makes macro expansion depend on backend emission.

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
- Macro execution can accidentally mutate compiler internals -> keep
  `cosmo-cte-sys` behind a serialized macro function input/output protocol.
- A loose Expr model can leak implementation details across phases -> keep
  `Expr[Untyped]` as an untyped source-expression value and expose typed facts
  only through bounded typer inspectors.

## Migration Plan

1. Preserve macro attributes in the parser/elaborator while keeping old
   unsupported-decorator diagnostics for non-macro profiles.
2. Add macro expansion artifacts and diagnostics behind a profile or package
   capability gate.
3. Implement compiler-hosted derive provider registration.
4. Add serialized macro function input/output records.
5. Add generated declaration builders and generated-source debug output.
6. Add deterministic tests for provider execution and CTE compile boundaries.
7. Enable the CLI derive provider as the first end-to-end consumer.

## Open Questions

- Whether macro provider packages should be declared through `macroDependencies`
  in `cosmo.json` or through ordinary dependencies plus provider metadata.
- How much of the reflection metadata should be available to ordinary
  compile-time code once self-hosted macros exist.
- Which textual syntax should be used for debugging generated declaration trees
  once the builder API becomes the primary output format.

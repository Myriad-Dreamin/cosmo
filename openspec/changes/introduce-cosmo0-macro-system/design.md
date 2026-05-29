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

**Non-Goals:**

- Add arbitrary expression quotation or function-like expression macros in the
  first slice.
- Allow macros to perform filesystem, command, network, or runtime effects.
- Require all macro providers to be written in Cosmo before the macro execution
  host exists.
- Make runtime reflection a dependency of generated parsers.
- Admit every existing full-language staging feature into cosmo0.
- Replace ordinary type checking with macro-time execution.

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

## Migration Plan

1. Preserve macro attributes in the parser/elaborator while keeping old
   unsupported-decorator diagnostics for non-macro profiles.
2. Add macro expansion artifacts and diagnostics behind a profile or package
   capability gate.
3. Implement compiler-hosted derive provider registration.
4. Add generated-source debug output and deterministic tests.
5. Enable the CLI derive provider as the first end-to-end consumer.

## Open Questions

- Whether macro provider packages should be declared through `macroDependencies`
  in `cosmo.json` or through ordinary dependencies plus provider metadata.
- Whether the first generated-source artifact should be source text, an AST
  tree, or a typed declaration builder API.
- How much of the reflection metadata should be available to ordinary
  compile-time code once self-hosted macros exist.

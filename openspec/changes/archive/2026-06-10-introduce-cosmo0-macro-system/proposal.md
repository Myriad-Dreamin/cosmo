## Why

Cosmo needs source-level code generation before libraries such as derived CLI
parsers, serializers, schema generators, and test discovery can be written in a
usable style. The current cosmo0 subset parses decorators for a few trusted
runtime hooks, but ordinary macros, derive expansion, and reflection metadata
remain outside the accepted source boundary.

## What Changes

- Introduce deterministic cosmo0 macro expansion points for source-level
  generated artifacts, including expression macro expansion during expression
  checking.
- Add accepted decorator storage for macro-owned declaration, field, variant,
  and function metadata instead of rejecting every non-extern decorator.
- Add custom derive support through `@derive(path)` on class-like declarations
  and sum types, with the first derive slice restricted to attaching existing
  trait implementations to existing items.
- Add the compile-time reflection metadata needed by derive macros: type name,
  fields, field types, variants, defaults, attributes, doc comments, visibility,
  and source spans.
- Define macro hygiene, generated-name behavior, generated span reporting, and
  duplicate/conflict diagnostics.
- Define that first-slice derive macros do not introduce new ordinary
  name-resolution bindings.
- Define a conservative macro execution boundary that is deterministic and does
  not grant arbitrary filesystem, command, network, or runtime side effects.
- Specify macro functions as pure computations over cosmo0-provided inputs; the
  compiler may rerun or cache them, and non-pure providers have undefined
  behavior.
- Define the compile-time computation model for macro providers, including the
  first compiler-hosted provider bridge and the later self-hosted interpreter
  boundary.
- Define `Expr[T = Untyped]` as the macro API's untyped source-expression value,
  with typed expression facts exposed only through typer-phase inspectors such
  as `Type.of(expr)`.
- Add documentation ownership for macros, derives, and reflection under
  `docs/cosmo/`.

## Capabilities

### New Capabilities

- `cosmo0-macro-expansion`: Defines macro expansion points, decorator
  preservation, expression-site expansion, generated artifacts, hygiene,
  deterministic expansion, and diagnostics.
- `cosmo0-derived-reflection`: Defines the reflection metadata and derive macro
  provider contract used by source-level `@derive(...)` macros.
- `cosmo0-compile-time-evaluation`: Defines the controlled compile-time
  evaluator/interpreter boundary, macro value model, untyped `Expr[Untyped]`
  boundary, typed inspector access, and deterministic execution rules.

### Modified Capabilities

- `cosmo0-spec-docs`: Extends the cosmo0 documentation skeleton so macros,
  derives, and reflection have an explicit owning spec file.

## Impact

- Parser and elaborator must preserve accepted macro decorators and reject only
  invalid or unsupported macro shapes.
- Expression checking needs to classify macro calls before ordinary call
  checking and then recheck generated `Expr[Untyped]` in the caller context.
  Declaration and derive macro output, when admitted, still needs integration
  before later compiler facts consume generated artifacts.
- Name resolution and diagnostics need generated-span and generated-name support.
- The first implementation may host macro providers in compiler infrastructure
  before self-hosted macro packages are available.
- `docs/cosmo/` must add macro/reflection documentation and the lightweight doc
  validation must account for it.

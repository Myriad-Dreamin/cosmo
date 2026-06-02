## Context

The broader macro-system proposal includes derive reflection and generated
declaration concepts, but the first implementation should avoid changing name
resolution. A derive provider that creates new declarations or members would
make later source depend on expansion output. By restricting derive output to
trait implementation attachments, the compiler can index declarations and
resolve names before derive expansion affects trait satisfaction.

## Goals / Non-Goals

**Goals:**

- Preserve `@derive(path)` attributes on supported item declarations.
- Resolve derive provider paths and selected trait paths deterministically.
- Provide stable reflection input for the already-indexed target item.
- Accept only generated trait implementation records for the target item.
- Validate generated implementations through the ordinary trait/impl checker.
- Keep ordinary name resolution unchanged by derive expansion.
- Feed derive-generated implementation facts into trait resolution and
  method-set selector resolution.
- Emit deterministic diagnostics and generated implementation summaries.

**Non-Goals:**

- Generate new top-level declarations, members, fields, variants, classes, or
  aliases.
- Make `Config.parse` or any other new name visible through derive expansion.
- Implement expression macro calls.
- Load self-hosted macro provider packages.
- Execute provider code through `cosmo-jit-sys`.
- Expose trusted typed expressions or compiler mutation handles to providers.

## Decisions

### Restrict Output To Trait Implementation Attachments

The first derive provider output is an implementation attachment:

```text
impl trait cli.Parser for Config { ... }
```

The trait and target item must already be resolved. Generated method bodies or
associated implementation data are validated as part of the implementation.

Alternative considered: let derive generate static methods or top-level helper
functions. That is useful for some libraries, but it introduces new bindings and
therefore affects name resolution and declaration ordering.

### Run After Declaration Indexing

Derive expansion runs after the declaration index, trait headers, class headers,
and supported item facts are available. It does not require ordinary function
body checking to finish before provider input is built.

Alternative considered: run derive during parsing. That would not have stable
trait/target identities or enough reflected field/type facts.

### Keep Name Resolution Non-Interfering

Because derive output cannot introduce names, the ordinary binding set remains
the same before and after derive expansion. Later code can use the generated
capability only through trait APIs whose names were already resolved.

Derive output still affects trait resolution. A generated implementation
attachment contributes an `ImplFact(trait for target)` to the implementation
fact index. Type checking that needs that trait evidence waits for the fact.
Method-like selector resolution also waits for `MethodSetFact(receiver,
selector)` when trait or extension lookup can contribute candidates, because
derive-generated impls may add candidates to that method set.

Alternative considered: allow derive output to add members, then rerun name
resolution. That makes the first implementation much larger and overlaps with
general declaration macro design.

### Use Compiler-Hosted Providers First

The implementation may start with compiler-hosted derive smoke providers. They
must still use the same serialized derive input/output shape that future
self-hosted providers will use.

Alternative considered: wait for self-hosted provider packages. That would
delay proving the derive attachment model and the CLI parser direction.

### Validate Generated Implementations Before Attachment

Provider output is not trusted. The compiler validates the target item, trait,
generated members, origin metadata, and duplicate implementation behavior before
attaching the impl to the checked package.

Alternative considered: attach provider output directly and rely on later
failures. That would produce weaker diagnostics and make invalid output harder
to isolate.

## Risks / Trade-offs

- Trait-implementation-only derive is less expressive -> keep this as the first
  slice and leave declaration-generating derives for a later capability.
- CLI-style libraries may want convenient static methods -> route first APIs
  through trait functions such as `cli.Parser.parse[Config](args)`.
- Compiler-hosted providers can become special cases -> keep their input/output
  protocol identical to future providers.
- Generated impl validation can overlap with ordinary impl checking -> reuse
  ordinary trait/impl validation where possible and keep derive-specific checks
  focused on provider output integrity.

## Migration Plan

1. Add the derive macro design document and OpenSpec capability.
2. Preserve and validate `@derive(path)` attributes on supported target items.
3. Add compiler-hosted smoke derive providers that return trait implementation
   records only.
4. Validate and attach generated implementations without changing the ordinary
   declaration index.
5. Add generated implementation summaries and diagnostics.
6. Leave declaration-generating derive output to a later proposal.

## Open Questions

- Whether the derive attribute path names the provider directly, the trait being
  derived, or a provider/trait pair selected by registry metadata.
- Which target item kinds are admitted in the first fixture set: classes only,
  or classes plus sum-like variants.

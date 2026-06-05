## Why

Cosmo needs a first derive macro implementation that is useful for library
features such as CLI parsing without forcing the compiler to support general
declaration-generation macros. The safe initial boundary is to let derive
providers attach implementations of existing traits to existing items, while
leaving ordinary name resolution unchanged.

## What Changes

- Add a gated derive macro implementation slice for `@derive(path)` attributes
  on supported item declarations.
- Resolve the derive provider and selected trait through the normal
  prefix-first resolver and provider registry.
- Build stable derive input from the existing target item metadata, admitted
  field/variant facts, attributes, defaults, doc comments, and source spans.
- Restrict first-slice provider output to trait implementation attachments for
  the existing target item.
- Reject generated top-level declarations, generated members, generated fields,
  generated variants, raw source text, and trusted typed expression artifacts.
- Validate generated implementation records and attach them after declaration
  indexing without changing the ordinary name-resolution binding set.
- Add derive-generated implementation facts to the trait-resolution and
  method-set indexes used by later type checking and selector resolution.
- Add deterministic diagnostics for unresolved providers, unsupported targets,
  unsupported traits, invalid output, duplicate implementations, and unconsumed
  attributes.

## Capabilities

### New Capabilities

- `cosmo0-derive-macros`: Defines the first derive macro implementation slice:
  provider selection, reflection input, trait-implementation-only output,
  attachment validation, generated implementation diagnostics,
  trait-resolution dependencies, and name-resolution non-interference.

### Modified Capabilities

- None.

## Impact

- Depends on `introduce-cosmo0-macro-system` for macro input/output,
  reflection, attribute preservation, and deterministic execution rules.
- Uses the derive boundary documented in `docs/cosmo/derive-macro.typ`.
- Adds provider registry entries or compiler-hosted smoke providers for focused
  derive fixtures.
- Touches attribute preservation, declaration indexing, trait implementation
  validation, trait-resolution fact indexing, generated origin metadata, and
  diagnostics.
- Does not require expression macro calls, self-hosted provider packages, or
  provider execution through `cosmo-cte-sys` in the first implementation.

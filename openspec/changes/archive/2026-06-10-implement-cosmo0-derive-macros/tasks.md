## 1. Attribute And Registry

- [x] 1.1 Preserve `@derive(path)` attributes on supported item declarations in
  the gated derive profile.
- [x] 1.2 Add deterministic provider registry entries or compiler-hosted smoke
  providers for derive fixtures.
- [x] 1.3 Resolve derive provider paths and selected trait paths through the
  prefix-first resolver.
- [x] 1.4 Report missing, duplicate, disabled, or invalid provider diagnostics.

## 2. Derive Input

- [x] 2.1 Build stable derive input records for already-indexed target items.
- [x] 2.2 Include target identity, item kind, module path, visibility, source
  spans, admitted type parameters, fields, variants, defaults, attributes, and
  doc comments.
- [x] 2.3 Include selected trait identity and trait requirement facts needed for
  generated implementation validation.
- [x] 2.4 Add deterministic display or fixture serialization for derive input.

## 3. Provider Output

- [x] 3.1 Accept only generated trait implementation records for the target item.
- [x] 3.2 Reject generated top-level declarations, members, fields, variants,
  aliases, raw source text, and typed expression artifacts.
- [x] 3.3 Track consumed attributes and reject unconsumed macro-owned
  attributes.
- [x] 3.4 Attach stable generated spans and derive origin metadata to generated
  implementation records.

## 4. Implementation Attachment

- [x] 4.1 Validate generated implementations through ordinary trait/impl
  checking where possible.
- [x] 4.2 Reject duplicate or conflicting trait implementations for the same
  target item.
- [x] 4.3 Build an implementation fact index from source impls and
  derive-generated implementation attachments.
- [x] 4.4 Attach valid generated implementations without adding names to the
  ordinary declaration index.
- [x] 4.5 Add method-set facts for receiver type and selector name where
  derive-generated impls can contribute trait or extension candidates.
- [x] 4.6 Add generated implementation summaries for tests and debugging.

## 5. Fixtures And Validation

- [x] 5.1 Add a positive class derive fixture that makes the class satisfy an
  existing trait through a generated implementation.
- [x] 5.2 Add coverage proving derive does not make a new method or top-level
  name resolvable.
- [x] 5.3 Add coverage proving trait uses wait for derive-generated impl facts
  without rerunning ordinary name resolution.
- [x] 5.4 Add coverage proving method-like selector resolution waits for
  method-set facts when trait or extension lookup can contribute candidates.
- [x] 5.5 Add unsupported target, unsupported trait, invalid output,
  duplicate-impl, unresolved provider, and unconsumed attribute fixtures.
- [x] 5.6 Verify repeated runs produce stable provider input, output,
  diagnostics, and summaries.
- [x] 5.7 Run the relevant cosmo0 tests and `scripts/check-scala-style.sh` if
  Scala sources are edited.

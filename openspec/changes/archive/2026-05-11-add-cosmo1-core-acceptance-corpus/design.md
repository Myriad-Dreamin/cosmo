## Context

cosmo0 does not exist in this repository as an implemented compiler package yet,
but its scope is already defined by the surrounding proposals: it should validate
compiler-shaped cosmo1 source without growing into full Cosmo. The acceptance
corpus provides one concrete source file that later cosmo0 phases can reuse as
the pipeline matures.

The corpus must therefore be representative enough to exercise the future subset
boundary while remaining additive and harmless to the current full compiler path.

## Goals / Non-Goals

**Goals:**

- Add one cosmo1-shaped source fixture under a cosmo0/cosmo1 validation path.
- Capture the canonical source conventions used by the fixture.
- Cover compiler-oriented data for spans, diagnostics, symbols, tokens, AST
  nodes, typed IDs, arenas, scopes, maps, sets, and parser state.
- Ensure the repository can discover the fixture and parse it with the shared
  parser today.
- Mark later cosmo0 phases as future consumers without requiring those phases in
  this change.

**Non-Goals:**

- Implement cosmo0 phases.
- Compile or type-check the corpus through the current full compiler.
- Implement cosmo1 behavior.
- Introduce user-defined generic declarations, traits, reflection, staging,
  closures, or full compiler logic in the corpus.

## Decisions

### Store the corpus under fixtures/cosmo0/cosmo1

The fixture lives at:

```text
fixtures/cosmo0/cosmo1/core-acceptance/cosmo1_core_acceptance.cos
```

This keeps it separate from existing full-compiler samples while making its role
explicit: cosmo1-shaped source validated by future cosmo0 phases.

Alternative considered: place it under `samples/`. That would make it look like a
full compiler sample and invite current compilation expectations that this change
intentionally avoids.

### Use a manifest and README for phase intent

The manifest records the file path, current validation level, future validation
phases, and source conventions. The README explains the same decisions for
humans.

Alternative considered: rely only on comments in the source file. That would make
test discovery and future phase ownership less explicit.

### Validate parser compatibility only

The current executable check ensures the manifest and source file exist and that
the shared parser accepts the corpus. It does not run type checking or
compilation.

Alternative considered: add the corpus to full compiler sample tests. That would
incorrectly require descriptor-backed cosmo0 standard types to exist in the full
compiler path.

## Risks / Trade-offs

- Corpus drifts from future cosmo0 syntax -> Mitigation: the parser test pins the
  current grammar, and later cosmo0 phases should update this file deliberately.
- Fixture is mistaken for working cosmo1 code -> Mitigation: manifest, README,
  and source header all mark it as an acceptance target.
- Future phases need multiple files instead of one corpus -> Mitigation: keep this
  file as the broad core target and let stage-specific changes add narrower
  package fixtures later.

## Migration Plan

1. Add the corpus, manifest, README, and parser/discovery test.
2. Reuse the corpus in cosmo0 subset elaboration once that phase exists.
3. Promote the same file through typing, LIR lowering, backend, and package
   validation as the corresponding cosmo0 features are implemented.
4. Keep current validation at parser/discovery level until those phases exist.

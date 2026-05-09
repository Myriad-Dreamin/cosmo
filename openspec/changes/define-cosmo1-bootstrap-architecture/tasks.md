## 1. Proposal Review

- [ ] 1.1 Review the cosmo1 file architecture capability and adjust package/file names.
- [ ] 1.2 Review the cosmo1 core type list and classify each type as stage-required or later.
- [ ] 1.3 Review the cosmo1 compiler capability list and identify dependencies on cosmo0/core0.
- [ ] 1.4 Review bootstrap stages and choose the first smoke source target.

## 2. Naming and Layout Decisions

- [ ] 2.1 Decide the cosmo1 package name and package metadata.
- [ ] 2.2 Decide whether `json_loader` is a required bridge file or a temporary experiment.
- [ ] 2.3 Decide which planned files can be grouped for the initial skeleton.
- [ ] 2.4 Decide which current Scala compiler artifacts must be mirrored first.

## 3. Core Type Decisions

- [ ] 3.1 Decide whether `Span`, `Diagnostic`, `Symbol`, `Arena<T>`, and `Id<T>` live in core0 std or cosmo1 modules.
- [ ] 3.2 Decide the deterministic map strategy for compiler state.
- [ ] 3.3 Decide the minimal `Vec<T>`, `Option<T>`, `Result<T, E>`, `Map<K, V>`, `Arena<T>`, and `Id<T>` APIs required by Stage 1 through Stage 3.
- [ ] 3.4 Decide whether `BigInt` is required in Stage 1 or can begin as numeric literal text.

## 4. Stage Planning

- [ ] 4.1 Define Stage 1 acceptance tests for source loading, spans, diagnostics, tokens, and lexing.
- [ ] 4.2 Define Stage 2 acceptance tests for syntax arenas and JSON AST loading.
- [ ] 4.3 Define Stage 3 acceptance tests for the native parser slice.
- [ ] 4.4 Define Stage 4 through Stage 6 acceptance tests for resolution, typing, and C++ emission.

## 5. Implementation Readiness

- [ ] 5.1 Create an implementation checklist mapping each proposed cosmo1 file to its first stage.
- [ ] 5.2 Create a dependency checklist mapping each cosmo1 file group to required cosmo0 language features.
- [ ] 5.3 Create a dependency checklist mapping each cosmo1 file group to required core0 standard types.
- [ ] 5.4 Confirm that no Stage 1 through Stage 3 requirement depends on user-defined generics or cosmo0 type-level computation.

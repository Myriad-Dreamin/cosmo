## Why

Cosmo is currently difficult to bootstrap because compiler-critical code quickly depends on full-language features such as type-level evaluation, user-defined generics, reflection, staging, and the full standard library. A Scala-implemented cosmo0 core gives the project a small, stable compilation path for bootstrap-sensitive code and, specifically, a host capable of compiling the staged cosmo1 compiler without requiring full self-hosting.

## What Changes

- Introduce cosmo0 as a Scala program that accepts a strict Cosmo subset and produces C++ for bootstrap-critical compiler components.
- Define a cosmo0 language subset that reuses the existing Cosmo parser but rejects unsupported full-language constructs with explicit diagnostics.
- Add sealed standard generic type constructors such as `Vec<T>`, `Option<T>`, `Result<T, E>`, `Map<K, V>`, `Set<T>`, `Arena<T>`, `Id<T>`, `Ptr<T>`, and `Box<T>` where the implementation is fixed by core0 standard descriptors.
- Add core0 runtime-backed scalar, utility, filesystem, JSON, numeric, and deterministic-collection capabilities needed by cosmo1 compiler stages.
- Add a separate cosmo0 compiler pipeline with subset checking, a low-level IR, simple type checking, and descriptor-driven C++ code generation.
- Add package-level checking/compilation and cosmo1 stage validation targets so cosmo0 can be measured against the compiler it must host.
- Keep full Cosmo, full `library/std`, reflection, staging, user-defined generics, trait constraints, and type-level programming outside cosmo0.

## Capabilities

### New Capabilities

- `cosmo0-language-subset`: Defines which Cosmo syntax and semantics are accepted by cosmo0 and which full-language features are rejected.
- `cosmo0-std-generics`: Defines sealed standard generic type constructors and the rule that cosmo0 must use their registered core0 implementations.
- `cosmo0-core-runtime`: Defines scalar, utility, runtime-backed, filesystem, JSON, numeric, and deterministic collection capabilities exposed to cosmo0 code.
- `cosmo0-compiler-pipeline`: Defines the Scala implementation pipeline, command surface, outputs, and validation behavior for cosmo0.

### Modified Capabilities

None.

## Impact

- Adds OpenSpec coverage for a new cosmo0 architecture before implementation.
- Future implementation is expected to add Scala sources under a cosmo0 namespace, a command entry point, core0 standard/runtime descriptors, tests, and staged bootstrap-focused sources that exercise cosmo1-style compiler code.
- No existing Cosmo language behavior is changed by this proposal; full Cosmo remains the experimental/high-level compiler path.

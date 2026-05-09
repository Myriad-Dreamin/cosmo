## 1. Corpus Scope

- [x] 1.1 Choose the acceptance corpus path under the cosmo0/cosmo1 validation area.
- [x] 1.2 Define the corpus conventions for canonical cosmo0 type application, references, mutability, and descriptor-backed standard types.
- [x] 1.3 List the cosmo1-style source forms the corpus is intended to exercise without implementing compiler behavior.

## 2. Corpus Source

- [x] 2.1 Add one Cosmo source file containing representative span, symbol, diagnostic, token, AST, arena, and parser-state declarations.
- [x] 2.2 Include standard generic uses for `Vec`, `Option`, `Result`, `Arena`, `Id`, `Map`, and `Set` where they are relevant to compiler-shaped data.
- [x] 2.3 Include expression examples for methods, `&self`, `&mut self`, mutation, field access, calls, assignments, conditionals, loops, variant construction, and match.
- [x] 2.4 Keep the file free of user-defined generics, host `Type`, traits, reflection, staging, closures, and full cosmo1 implementation logic.

## 3. Corpus Validation

- [x] 3.1 Add a lightweight test or manifest entry that ensures the corpus file is discoverable by the repository test infrastructure.
- [x] 3.2 Mark the corpus as an acceptance target for later cosmo0 phases rather than requiring full cosmo0 compilation immediately.
- [x] 3.3 Document which later phases are expected to consume the corpus as parse, elaboration, typing, LIR, backend, and package validation mature.

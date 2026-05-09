## 1. Specification Baseline

- [ ] 1.1 Review and accept the updated cosmo0 OpenSpec proposal, design, and capability specs.
- [ ] 1.2 Decide the canonical cosmo0 type application syntax for standard generics.
- [ ] 1.3 Decide whether `Ptr<T>` and `Ref<T>` are descriptor generics or primitive type forms.
- [ ] 1.4 Decide whether `Vec<T>` and related containers lower to direct STL types or a core0 wrapper API.
- [ ] 1.5 Decide whether `Span`, `Diagnostic`, `Symbol`, `Arena<T>`, and `Id<T>` live in core0 descriptors, cosmo1 modules, or a split design.
- [ ] 1.6 Decide whether `Map<K, V>` is deterministic by default or whether deterministic behavior uses a separate `OrderedMap<K, V>`.

## 2. Compiler Skeleton

- [ ] 2.1 Add the cosmo0 Scala package/module structure without changing the full Cosmo compiler path.
- [ ] 2.2 Add cosmo0 entry points for file-level check and compile operations.
- [ ] 2.3 Add cosmo0 entry points for package-level check and compile operations.
- [ ] 2.4 Wire cosmo0 parsing to the existing parser and require subset checking before lowering.
- [ ] 2.5 Add test fixtures for valid and invalid cosmo0 source snippets.

## 3. Language Subset Checker

- [ ] 3.1 Implement acceptance checks for modules, acyclic imports, public declarations, non-generic classes, variants, methods, variables, simple type aliases, and core expressions.
- [ ] 3.2 Implement acceptance checks for variant construction, variant payload matching, primitive matching, wildcard branches, and initial match lowering constraints.
- [ ] 3.3 Implement acceptance checks for `&T`, `&mut T`, `&self`, `&mut self`, mutable locals, mutable fields, and descriptor-backed mutable container access.
- [ ] 3.4 Implement restricted `for` support for descriptor-defined iterable targets such as `Vec<T>`.
- [ ] 3.5 Reject user-defined generic classes, functions, traits, impls, and compile-time type parameters.
- [ ] 3.6 Reject `Type`, type functions, dependent type expressions, trait constraints, HKT, reflection, staging, macros, quotation, paste operations, closures, lambdas, and unsupported higher-order collection APIs.
- [ ] 3.7 Emit source-location diagnostics for every rejected construct covered by the specs.

## 4. Core Runtime Descriptors

- [ ] 4.1 Add descriptor coverage for core scalar and text types, including `Unit`, `Bool`, integer widths, `usize`, `Byte`, `Char`, `String`, string slices, and byte buffers.
- [ ] 4.2 Add descriptor-defined string inspection APIs needed by lexing and diagnostics.
- [ ] 4.3 Add a `StringBuilder` descriptor for diagnostics, pretty-printing, and C++ emission.
- [ ] 4.4 Add staged `JsonValue` and JSON parse descriptors for AST JSON and package metadata bridges.
- [ ] 4.5 Add staged `Path` and `Fs` descriptors for reading source files and writing generated outputs.
- [ ] 4.6 Add a staged `Command` descriptor for later build/link/run integration.
- [ ] 4.7 Add lossless numeric literal support, beginning with literal text preservation and leaving `BigInt`/`BigDecimal` descriptors for the stage that needs arithmetic.

## 5. Standard Generic Descriptors

- [ ] 5.1 Add a core0 descriptor registry for `Vec<T>`, `Option<T>`, `Result<T, E>`, `Map<K, V>`, `Set<T>`, `Arena<T>`, `Id<T>`, `Ptr<T>`, and `Box<T>` as needed.
- [ ] 5.2 Implement arity and nested type-application validation for registered standard constructors.
- [ ] 5.3 Prevent user declarations from overriding registered core0 standard constructors.
- [ ] 5.4 Add descriptor-defined method signatures for required `Vec<T>`, `Option<T>`, and `Result<T, E>` operations.
- [ ] 5.5 Add descriptor-defined method signatures for `Arena<T>` and `Id<T>` operations, including `alloc`, `get`, `getMut`, and `len`.
- [ ] 5.6 Add descriptor-defined method signatures for `Map<K, V>` and `Set<K>` operations, including key-type validation and deterministic iteration behavior.

## 6. IR and Type Checking

- [ ] 6.1 Define the cosmo0 low-level IR for modules, classes, variants, functions, statements, expressions, references, and supported types.
- [ ] 6.2 Lower accepted syntax into cosmo0 IR without relying on full Cosmo type-level IR nodes.
- [ ] 6.3 Implement type checking for local variables, assignments, field access, method calls, function calls, returns, aliases, variant constructors, and match branches.
- [ ] 6.4 Implement descriptor type-parameter substitution for standard generic methods such as `Vec<T>.push`, `Arena<T>.alloc`, and `Map<K, V>.get`.
- [ ] 6.5 Add type diagnostics for return mismatch, assignment mismatch, invalid receiver mutability, invalid map key types, and invalid descriptor method calls.

## 7. C++ Code Generation

- [ ] 7.1 Implement C++ code generation for accepted cosmo0 IR and registered standard generics.
- [ ] 7.2 Implement stable namespace, symbol, and output ordering for repeated builds.
- [ ] 7.3 Implement runtime descriptor include/support-code emission.
- [ ] 7.4 Implement multi-module package output using the selected header/source or single-translation-unit strategy.
- [ ] 7.5 Add tests proving repeated generation of the same package is deterministic.

## 8. cosmo1 Stage Validation

- [ ] 8.1 Define the first cosmo1 stage descriptor set for source loading, spans, diagnostics, tokens, lexing, and text output.
- [ ] 8.2 Add a cosmo1-style Stage 1 smoke source that uses only accepted cosmo0 constructs and Stage 1 descriptors.
- [ ] 8.3 Add a check-package validation test for the Stage 1 smoke source.
- [ ] 8.4 Add a compile-package validation test that emits C++ for the Stage 1 smoke source.
- [ ] 8.5 Add negative validation tests proving accidental user-defined generics, host `Type`, reflection, staging, closures, and unsupported higher-order APIs are rejected in cosmo1 stage sources.

## 9. Documentation

- [ ] 9.1 Document the cosmo0 subset as the host language for staged cosmo1 compiler code.
- [ ] 9.2 Document the descriptor registry and core0 runtime staging model.
- [ ] 9.3 Document the first cosmo0-supported cosmo1 bootstrap workflow.

## ADDED Requirements

### Requirement: Required core0 Scalar Types

cosmo1 SHALL be expressible using core0 scalar types for unit, booleans, signed and unsigned integers, pointer-sized integers, bytes, characters, strings, string slices, and byte buffers.

#### Scenario: Compiler data uses scalar types

- **WHEN** cosmo1 defines spans, token offsets, source identifiers, counters, and textual names
- **THEN** those definitions use core0 scalar types such as `Unit`, `Bool`, `i32`, `i64`, `u32`, `u64`, `usize`, `Byte`, `Char`, `String`, `Str`, and byte-buffer types

### Requirement: Required core0 Generic Containers

cosmo1 SHALL be expressible using sealed core0 standard generics for optional values, recoverable results, vectors, maps, sets, arenas, typed IDs, boxed values, pointers, and references.

#### Scenario: Compiler data uses standard generics

- **WHEN** cosmo1 defines AST lists, lookup tables, scopes, interners, arenas, and fallible operations
- **THEN** those definitions can use `Option<T>`, `Result<T, E>`, `Vec<T>`, `Map<K, V>`, `Set<T>`, `Arena<T>`, `Id<T>`, `Box<T>`, `Ptr<T>`, `&T`, and `&mut T`

#### Scenario: User-defined generics remain unnecessary

- **WHEN** cosmo1 needs typed identifiers such as `ExprId`, `TyId`, `DefId`, or `ScopeId`
- **THEN** it can define aliases over sealed standard generics rather than declaring user-defined generic classes

### Requirement: Required core0 Utility Types

cosmo1 SHALL have access to utility types for string construction, paths, filesystem operations, process execution, JSON values, arbitrary precision integers, and deterministic collection behavior.

#### Scenario: Driver and codegen use utility types

- **WHEN** cosmo1 reads packages, writes generated C++, emits diagnostics, or invokes external tools
- **THEN** it can use types or modules equivalent to `StringBuilder`, `Path`, `Fs`, `Command`, `JsonValue`, `BigInt`, `BigDecimal`, and deterministic `Map` behavior

### Requirement: cosmo1 Source and Diagnostic Types

cosmo1 SHALL define source and diagnostic data types for source identifiers, spans, source files, source maps, diagnostic severity, diagnostic labels, and fix-it hints.

#### Scenario: Diagnostic data is representable

- **WHEN** cosmo1 reports a parse, resolution, type, or codegen error
- **THEN** it can attach severity, message, span labels, and optional fix-it information to the diagnostic

### Requirement: cosmo1 Syntax Types

cosmo1 SHALL define syntax data types for tokens, token kinds, AST nodes, items, declarations, expressions, patterns, parameters, imports, classes, traits, impls, and cases.

#### Scenario: Full syntax is representable as data

- **WHEN** cosmo1 parses or loads full Cosmo source
- **THEN** it can represent constructs including functions, variables, classes, traits, impls, type aliases, imports, calls, selection, match expressions, template literals, patterns, and case variants as compiler data

### Requirement: cosmo1 Semantic Types

cosmo1 SHALL define semantic data types for symbols, names, scopes, definitions, modules, packages, type expressions, canonical types, class metadata, trait metadata, impl metadata, function signatures, and constraints.

#### Scenario: Full type system is representable as data

- **WHEN** cosmo1 type-checks full Cosmo features
- **THEN** it can represent generic declarations, type applications, universe/type values, trait requirements, impl candidates, subtype relations, and normalized types as ordinary compiler data

### Requirement: cosmo1 Evaluation and IR Types

cosmo1 SHALL define data types for compile-time values, evaluation environments, high-level IR, typed IR, lowered codegen IR, and generated C++ structures.

#### Scenario: Compile-time behavior is representable

- **WHEN** cosmo1 evaluates type-level expressions or lowers typed source
- **THEN** it can represent compile-time values, evaluation state, typed expressions, lowered declarations, and C++ emission structures without requiring cosmo0 type-level computation

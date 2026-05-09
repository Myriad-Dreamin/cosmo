## ADDED Requirements

### Requirement: cosmo0 Host Capabilities Needed By cosmo1

cosmo1 SHALL require only cosmo0 host-language capabilities for ordinary compiler implementation: modules, imports, non-generic classes, variants, pattern matching, functions, methods, simple aliases, mutation, references, standard generic type application, control flow, and C++ emission.

#### Scenario: Full feature implemented as data

- **WHEN** cosmo1 implements a full Cosmo feature such as user-defined generics, traits, or `Type`
- **THEN** the feature is represented by cosmo1 data structures and algorithms rather than by an equivalent cosmo0 host-language feature

### Requirement: Source Loading and Lexing

cosmo1 SHALL provide compiler capabilities for source loading, source mapping, tokenization, and lexical diagnostics.

#### Scenario: Source is tokenized

- **WHEN** cosmo1 receives a source file
- **THEN** it can load the text, assign source identifiers, produce spans, tokenize the content, and report lexical errors

### Requirement: Parsing and AST Loading

cosmo1 SHALL provide compiler capabilities for native parsing and transitional JSON AST loading.

#### Scenario: Native parser produces AST

- **WHEN** cosmo1 parses source supported by the current bootstrap stage
- **THEN** it produces arena-backed syntax nodes with spans

#### Scenario: JSON loader bridges existing parser output

- **WHEN** cosmo1 receives AST JSON from the existing Scala parser
- **THEN** it can load the JSON into cosmo1 syntax data for downstream stages

### Requirement: Package and Module Resolution

cosmo1 SHALL provide compiler capabilities for package metadata loading, module graph construction, import resolution, and cycle diagnostics.

#### Scenario: Package graph is built

- **WHEN** cosmo1 compiles a package with imports
- **THEN** it loads package metadata, resolves module paths, builds a module graph, and reports missing or cyclic dependencies

### Requirement: Name Resolution

cosmo1 SHALL provide compiler capabilities for symbol interning, scope construction, definition allocation, qualified path resolution, and unresolved-name diagnostics.

#### Scenario: Names resolve to definitions

- **WHEN** a parsed module references imported, local, class, or member names
- **THEN** cosmo1 resolves those names to definition identifiers or reports diagnostics

### Requirement: Type Checking and Normalization

cosmo1 SHALL provide compiler capabilities for type expression lowering, type checking, type normalization, subtyping, generic type representation, trait/impl candidate representation, and typed IR construction.

#### Scenario: Typed IR is produced

- **WHEN** source passes name resolution and type checking
- **THEN** cosmo1 produces typed IR containing expression types, resolved definitions, and diagnostics for invalid programs

### Requirement: Compile-Time Evaluation

cosmo1 SHALL provide compiler capabilities for representing and evaluating compile-time values needed by full Cosmo.

#### Scenario: Type-level expression is evaluated by cosmo1

- **WHEN** full Cosmo source contains type-level expressions or compile-time values
- **THEN** cosmo1 evaluates them using its own value representation and evaluator rather than cosmo0 host-language type evaluation

### Requirement: C++ Code Generation and Linking

cosmo1 SHALL provide compiler capabilities for lowering typed IR to C++ emission structures, generating C++ source/header artifacts, dependency files, and optional linker/build invocations.

#### Scenario: C++ artifact is emitted

- **WHEN** a module reaches code generation
- **THEN** cosmo1 emits deterministic C++ output and records the artifacts needed for build integration

### Requirement: Artifacts and Incremental Metadata

cosmo1 SHALL provide compiler capabilities for dependency files, IR/cache metadata, and scope JSON or equivalent language-service artifacts.

#### Scenario: Artifact metadata is written

- **WHEN** cosmo1 compiles a module
- **THEN** it can write dependency and scope metadata needed by later builds or tooling

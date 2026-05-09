## ADDED Requirements

### Requirement: Dedicated cosmo1 Package

cosmo1 SHALL be planned as a dedicated package separate from the current Scala compiler and existing exploratory compiler sources.

#### Scenario: Package boundary is defined

- **WHEN** cosmo1 implementation begins
- **THEN** the implementation has a package root such as `packages/cosmo1/` with its own package metadata and source tree

#### Scenario: Existing compiler remains separate

- **WHEN** cosmo1 files are added
- **THEN** the existing Scala compiler path remains available and is not replaced by default

### Requirement: Driver Files

cosmo1 SHALL include driver files for configuration, session state, compilation orchestration, and diagnostics.

#### Scenario: Driver layout is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes files corresponding to `main`, `driver/config`, `driver/session`, `driver/compile`, and `driver/diagnostic`

### Requirement: Source and Lexing Files

cosmo1 SHALL include source management and lexing files for source content, spans, source maps, tokens, and lexical scanning.

#### Scenario: Source and lexing layout is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes files corresponding to `source/source`, `source/span`, `source/source_map`, `lex/token`, and `lex/lexer`

### Requirement: Syntax Files

cosmo1 SHALL include syntax files for AST definitions, native parsing, transitional JSON AST loading, and syntax pretty-printing.

#### Scenario: Syntax layout is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes files corresponding to `syntax/ast`, `syntax/parser`, `syntax/json_loader`, and `syntax/pretty`

### Requirement: Package and Name Files

cosmo1 SHALL include package/module loading and name-resolution files for package metadata, module graph construction, symbols, scopes, and resolution.

#### Scenario: Package and name layout is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes files corresponding to `package/meta`, `package/loader`, `package/module_graph`, `names/symbol`, `names/scope`, and `names/resolve`

### Requirement: Type, Eval, and IR Files

cosmo1 SHALL include type-system, compile-time evaluation, and IR files for type expressions, semantic types, type environments, checking, subtyping, normalization, compile-time values, interpreter behavior, and lowered IRs.

#### Scenario: Type eval IR layout is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes files corresponding to `types/type_expr`, `types/ty`, `types/env`, `types/check`, `types/subtype`, `types/normalize`, `eval/value`, `eval/interpreter`, `eval/builtins`, `ir/hir`, `ir/tir`, and `ir/lower`

### Requirement: Codegen Link Cache Files

cosmo1 SHALL include C++ code generation, linking, and artifact/cache files.

#### Scenario: Codegen link cache layout is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes files corresponding to `codegen/cpp/ast`, `codegen/cpp/emit`, `codegen/cpp/names`, `codegen/cpp/runtime`, `link/artifact`, `link/cmake`, `link/command`, `cache/depfile`, `cache/ir_cache`, and `cache/scope_json`

### Requirement: Smoke Test Files

cosmo1 SHALL include smoke-oriented test source files that can be compiled by cosmo0 before full compiler parity exists.

#### Scenario: Smoke file is present

- **WHEN** the cosmo1 package skeleton is created
- **THEN** it includes at least one smoke-oriented source file or test fixture that exercises the current bootstrap stage

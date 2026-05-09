## ADDED Requirements

### Requirement: Stage 0 Architecture Acceptance

cosmo1 SHALL first accept an architecture-only stage that creates the package skeleton and validates that every planned file group maps to a capability.

#### Scenario: Skeleton is reviewable

- **WHEN** Stage 0 is complete
- **THEN** reviewers can inspect the package skeleton and confirm that driver, source, syntax, package, name, type, eval, IR, codegen, link, cache, and smoke areas exist

### Requirement: Stage 1 Source Token Diagnostic

cosmo1 SHALL provide an early stage for source loading, spans, diagnostics, token definitions, and lexing.

#### Scenario: Source is lexed through cosmo0-compiled cosmo1

- **WHEN** Stage 1 is complete
- **THEN** a cosmo0-compiled cosmo1 component can tokenize a small source file and print or record diagnostics with spans

### Requirement: Stage 2 Syntax Arena JSON Loader

cosmo1 SHALL provide a stage for syntax arenas and JSON AST loading compatible enough to consume current parser JSON for selected samples.

#### Scenario: JSON AST is loaded

- **WHEN** Stage 2 is complete
- **THEN** cosmo1 can load AST JSON for a selected sample into arena-backed syntax data

### Requirement: Stage 3 Native Parser Slice

cosmo1 SHALL provide a native parser slice for a small but useful source subset.

#### Scenario: Native parser handles smoke source

- **WHEN** Stage 3 is complete
- **THEN** cosmo1 can parse a smoke source containing imports, variables, functions, classes, calls, and basic control flow

### Requirement: Stage 4 Resolution and Package Graph

cosmo1 SHALL provide a stage for package metadata, module graph construction, imports, symbol interning, scopes, and name resolution.

#### Scenario: Names resolve in a small package

- **WHEN** Stage 4 is complete
- **THEN** cosmo1 can resolve names across a small package and report unresolved names with spans

### Requirement: Stage 5 Type Checking Slice

cosmo1 SHALL provide a stage for type representation and a narrow type-checking slice sufficient for the smoke source.

#### Scenario: Smoke source is typed

- **WHEN** Stage 5 is complete
- **THEN** cosmo1 can produce typed IR for the smoke source and report type mismatches

### Requirement: Stage 6 C++ Emission Slice

cosmo1 SHALL provide a stage for deterministic C++ emission from typed IR for the smoke source.

#### Scenario: Smoke source emits C++

- **WHEN** Stage 6 is complete
- **THEN** cosmo1 emits C++ for the smoke source without relying on the existing Scala full compiler for downstream compilation

### Requirement: Stage 7 Full Feature Growth

cosmo1 SHALL grow full Cosmo feature support after the staged compiler skeleton is working.

#### Scenario: Full features are added after core pipeline

- **WHEN** work begins on full generics, trait resolution, type-level evaluation, reflection, or staging
- **THEN** those features are added as cosmo1 compiler data and algorithms after the staged source-to-C++ pipeline exists

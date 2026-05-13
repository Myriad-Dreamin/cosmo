## ADDED Requirements

### Requirement: Minimal Option Result Vec Capability

cosmo0 SHALL expose a `core0.option-result-vec` standard capability for Stage 1 optional values, recoverable results, and ordered mutable buffers.

#### Scenario: Stage profile requires the capability

- **WHEN** the `cosmo1.stage1` profile is validated
- **THEN** it requires the `core0.option-result-vec` std capability
- **AND** missing availability for that capability is reported as a missing std capability

### Requirement: Sealed Standard Type Applications

cosmo0 SHALL accept `Option[T]`, `Result[T, E]`, and `Vec[T]` only as sealed registered standard type applications with fixed arity.

#### Scenario: Registered applications are accepted

- **WHEN** source declares values of type `Option[Token]`, `Result[Token, Diagnostic]`, and `Vec[Token]`
- **THEN** cosmo0 accepts the applications as standard core0 types

#### Scenario: User-defined generic declarations remain rejected

- **WHEN** source declares a user-defined generic class or function while using this capability
- **THEN** cosmo0 rejects the declaration before lowering

### Requirement: Minimal Vec API

cosmo0 SHALL type-check and lower the Stage 1 `Vec[T]` API for construction, append, indexed access, indexed mutation, length, and emptiness checks.

#### Scenario: Vec operations lower through the transitional implementation

- **WHEN** source constructs `Vec[i32]`, pushes values, gets and sets by `usize` index, and reads `len`, `size`, or `is_empty`
- **THEN** cosmo0 assigns the descriptor-defined result types and lowers the calls through the registered standard implementation

### Requirement: Option And Result Variants

cosmo0 SHALL type-check and lower construction and matching for `Option[T]` and `Result[T, E]` variants.

#### Scenario: Option and Result match arms bind payloads

- **WHEN** source constructs `Option[T]::Some`, `Option[T]::None`, `Result[T, E]::Ok`, or `Result[T, E]::Err` and matches the value
- **THEN** cosmo0 validates payload arity and types and exposes bound payloads with their corresponding type parameters

### Requirement: Stage 1 Component Usage

cosmo1 Stage 1 source SHALL be able to store diagnostics in `Vec[Diagnostic]` and produce or buffer lexer tokens in `Vec[Token]`.

#### Scenario: Diagnostics and lexer buffers compile

- **WHEN** the `packages/cosmoc` Stage 1 package is checked and compiled
- **THEN** driver diagnostics use `Vec[Diagnostic]`
- **AND** lexer state produces and buffers `Vec[Token]`

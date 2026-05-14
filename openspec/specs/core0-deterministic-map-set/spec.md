# core0-deterministic-map-set Specification

## Purpose
TBD - created by archiving change add-core0-deterministic-map-set. Update Purpose after archive.
## Requirements
### Requirement: Deterministic Map And Set APIs

cosmo0 SHALL expose `Map[K, V]` and `Set[T]` as deterministic source-facing standard collection APIs under the `core0.map-set` capability.

#### Scenario: Map insertion and lookup are available

- **WHEN** cosmo0 source creates `Map[String, V]`, inserts keys, checks containment, and looks up values
- **THEN** the source type-checks and lowers through the `Map[K, V]` standard API
- **AND** `insert` returns the previous value as `Option[V]`
- **AND** `get` returns the current value as `Option[V]`

#### Scenario: Set insertion and containment are available

- **WHEN** cosmo0 source creates `Set[String]`, inserts values, and checks containment
- **THEN** the source type-checks and lowers through the `Set[T]` standard API

#### Scenario: Iteration order is deterministic

- **WHEN** source iterates a `Map[K, V]` or `Set[K]`
- **THEN** map iteration yields keys in key-sorted order
- **AND** set iteration yields values in key-sorted order
- **AND** repeated backend output for the same source remains stable

### Requirement: Map And Set Key Types

cosmo0 SHALL restrict deterministic map and set keys to the explicitly supported key set until trait-based hashing or ordering exists.

#### Scenario: Supported key types are accepted

- **WHEN** source uses `String`, primitive integer types, or `Id[T]` as a `Map` key or `Set` item type
- **THEN** type checking accepts the collection type

#### Scenario: Unsupported key types are rejected

- **WHEN** source uses a user class, floating-point value, reference, or other unsupported type as a `Map` key or `Set` item type
- **THEN** type checking reports `cosmo0.type.unsupported-map-key`

### Requirement: Cosmo1 Package And Name Components Use Deterministic Collections

cosmo1 SHALL provide initial package graph and name scope source components that store symbol-indexed data in deterministic maps and sets.

#### Scenario: Package graph compiles with deterministic collections

- **WHEN** the cosmo1 package graph source is compiled with symbol support and `core0.map-set`
- **THEN** module lookup and root-module tracking use `Map[Symbol, ModuleId]` and `Set[Symbol]`

#### Scenario: Name scope compiles with deterministic collections

- **WHEN** the cosmo1 name scope source is compiled with symbol support and `core0.map-set`
- **THEN** definition lookup and exported-name tracking use `Map[Symbol, DefId]` and `Set[Symbol]`


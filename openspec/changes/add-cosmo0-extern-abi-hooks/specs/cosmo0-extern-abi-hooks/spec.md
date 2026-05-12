## ADDED Requirements

### Requirement: Trusted Extern ABI Metadata

cosmo0 SHALL allow only trusted core0/std declarations to attach extern ABI metadata. Extern metadata SHALL name the ABI, target runtime symbol, and backend requirements without adding a descriptor family for the same runtime domain. C++ extern target symbols SHALL be represented as structured qualified symbols rather than arbitrary C++ call expressions.

#### Scenario: Trusted std declaration lowers to extern metadata

- **WHEN** a trusted std declaration such as `println(value: String): Unit` has no cosmo0 body
- **THEN** lowering records an extern binding using `cosmo0.extern.v0`
- **AND** lowering emits direct calls to the trusted extern declaration instead of a `Runtime` descriptor operation

#### Scenario: Namespaced C++ extern target is emitted absolutely

- **WHEN** a trusted extern binding targets the C++ symbol `cosmo0_runtime::println`
- **THEN** the C++ backend emits the call as `::cosmo0_runtime::println(...)`
- **AND** backend runtime requirement tracking records the canonical runtime symbol `cosmo0_runtime::println`

#### Scenario: Untrusted bodyless declaration is rejected

- **WHEN** a source declaration has no body and no trusted extern ABI metadata
- **THEN** lowering returns a compile-phase missing-extern-binding diagnostic

### Requirement: Backend Runtime Requirement Tracking

cosmo0 backends SHALL track runtime symbol, include, support-library, and descriptor requirements as backend requirements. Runtime symbol requirements SHALL remain separate from descriptor operations.

#### Scenario: Extern call records runtime requirements

- **WHEN** the C++ backend emits a call to a trusted extern binding
- **THEN** the backend records the required runtime symbol and include requirement
- **AND** descriptor requirements remain represented as descriptor requirements

### Requirement: Missing Extern Runtime Symbols

The C++ backend SHALL diagnose extern bindings whose runtime symbols are unavailable for the selected backend.

#### Scenario: Missing runtime symbol is diagnosed

- **WHEN** a checked LIR module calls an extern-bound function with an unsupported runtime symbol
- **THEN** C++ backend emission fails with a missing-extern-runtime-symbol diagnostic

### Requirement: Cosmo1 Extern Std Smoke

The cosmo1 smoke corpus SHALL include a small cosmo0 package that calls an extern-backed std function without requiring filesystem or command APIs.

#### Scenario: Smoke package calls std output API

- **WHEN** the cosmo1 extern std smoke package is checked and compiled
- **THEN** package validation succeeds
- **AND** backend output calls the extern-backed std output runtime symbol
- **AND** the smoke source does not depend on filesystem or command APIs

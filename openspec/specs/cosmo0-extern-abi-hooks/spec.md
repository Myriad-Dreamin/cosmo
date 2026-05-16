# cosmo0-extern-abi-hooks Specification

## Purpose
Define the trusted extern ABI hooks that let core0/std declarations bind to backend runtime symbols, direct fixed-arity C functions, ordered file-level C includes, and extern-backed smoke validation without expanding descriptor families.
## Requirements
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

#### Scenario: Trusted filesystem read lowers to extern metadata

- **WHEN** the trusted `core0.path-fs` declaration `Fs.read_to_string(path): Result<String, IoError>` has no cosmo0 body
- **THEN** lowering records an extern binding using `cosmo0.extern.v0`
- **AND** lowering emits a call to the trusted `cosmo0_runtime::read_file` filesystem runtime symbol instead of a `Filesystem` or `Path` descriptor operation

#### Scenario: Command run binding is trusted std

- **WHEN** the trusted `core0.command` declaration `command_run(command: &Command): Result[CommandResult, CommandError]` has no cosmo0 body
- **THEN** cosmo0 attaches `cosmo0.extern.v0` metadata for `::cosmo0_runtime::command_run`
- **AND** the binding is accepted only as repository-owned core0/std API metadata
- **AND** arbitrary packages cannot introduce new bodyless command execution declarations

#### Scenario: Command binding emits runtime requirements

- **WHEN** the C++ backend emits a call to the trusted `core0.command` run binding
- **THEN** it records the runtime symbol `cosmo0_runtime::command_run`
- **AND** it records the process runtime include and support requirements required by the selected backend

### Requirement: Backend Runtime Requirement Tracking

cosmo0 backends SHALL track runtime symbol, include, support-library, and descriptor requirements as backend requirements. Runtime symbol requirements SHALL remain separate from descriptor operations.

#### Scenario: Extern call records runtime requirements

- **WHEN** the C++ backend emits a call to a trusted extern binding
- **THEN** the backend records the required runtime symbol and include requirement
- **AND** descriptor requirements remain represented as descriptor requirements

#### Scenario: File-level C includes are emitted in source order

- **WHEN** a trusted source file declares `@include("a.h");` before `@include("b.h", kind = "c");`
- **THEN** the C++ backend emits `#include <a.h>` before `#include <b.h>`
- **AND** both headers are recorded as include backend requirements

#### Scenario: C include kind is inferred from header extension

- **WHEN** a trusted source file declares `@include("stdio.h");`
- **THEN** elaboration infers `kind = "c"` from the `.h` extension
- **AND** the C++ backend emits `#include <stdio.h>`

#### Scenario: Filesystem read records runtime requirements

- **WHEN** the C++ backend emits a call to the trusted `core0.path-fs` read binding
- **THEN** the backend records the required filesystem runtime symbol and include requirement
- **AND** descriptor requirements remain unchanged

### Requirement: Direct C Function Correspondence

Trusted `@extern("c")` declarations SHALL correspond to callable C ABI function symbols with direct positional parameter passing. The declaration SHALL NOT represent C macros or arbitrary C/C++ call expressions.

#### Scenario: Fixed-arity C function binding maps positionally

- **WHEN** a trusted declaration is written after `@include("stdio.h");` as `@extern("c", name = "puts") def puts(text: CString): i32`
- **THEN** the extern metadata names the C symbol `puts`
- **AND** cosmo argument `text` is emitted as the first C function argument
- **AND** the file-level include makes the header requirement explicit without using a function-level include argument

#### Scenario: Function-level include argument is rejected

- **WHEN** a trusted declaration is written as `@extern("c", include = "<stdio.h>") def puts(text: CString): i32`
- **THEN** elaboration fails with an invalid extern decorator diagnostic
- **AND** the diagnostic directs the source to use a file-level `@include(...);` directive

#### Scenario: Variadic C function binding is deferred

- **WHEN** a trusted declaration needs to correspond to a C prototype such as `int printf(const char *format, ...)`
- **THEN** the direct-binding subset documents the C correspondence
- **AND** support for `...`, signature candidates, format-string validation, and template-like `Any` extern parameters is left to a later accepted capability

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

### Requirement: Direct C Extern Support Library Metadata

Trusted direct C extern declarations SHALL be able to record a support-library id that identifies the Rust-backed support library required to satisfy the C ABI symbol. The id SHALL follow the shared support-library lowercase kebab-case convention and SHALL lower to a `support-library:<id>` backend requirement.

#### Scenario: Direct C extern records support library requirement

- **WHEN** a trusted declaration uses `@extern("c", name = "cosmo_support_smoke_add", supportLibrary = "support-smoke")`
- **THEN** elaboration preserves the `support-smoke` support-library id in extern metadata
- **AND** lowering records a backend requirement `support-library:support-smoke`

#### Scenario: Invalid support library id is rejected

- **WHEN** a trusted declaration uses an invalid support-library id such as `SupportSmoke`
- **THEN** elaboration fails with `cosmo0.elaborate.invalid-extern`
- **AND** the diagnostic names the invalid support-library id convention


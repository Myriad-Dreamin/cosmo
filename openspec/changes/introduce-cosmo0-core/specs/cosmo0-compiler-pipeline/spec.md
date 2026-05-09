## ADDED Requirements

### Requirement: Scala-Implemented Pipeline

cosmo0 SHALL be implemented as a Scala compiler path with explicit phases for parsing, subset checking, lowering to cosmo0 IR, type checking, and C++ code generation.

#### Scenario: Pipeline compiles accepted source

- **WHEN** a source file passes parsing, subset checking, and cosmo0 type checking
- **THEN** cosmo0 emits C++ for that source file

#### Scenario: Pipeline stops on subset error

- **WHEN** a source file contains a feature outside the cosmo0 subset
- **THEN** cosmo0 stops before lowering and reports the subset diagnostic

### Requirement: Independent cosmo0 IR

cosmo0 SHALL lower accepted syntax into an IR that does not require full Cosmo type-level evaluation, dependent type normalization, trait resolution, reflection, or staging.

#### Scenario: Core declaration lowers to cosmo0 IR

- **WHEN** source declares a non-generic class with concrete fields and methods
- **THEN** cosmo0 lowers it into cosmo0 IR entities for classes, fields, methods, expressions, and statements

#### Scenario: Full Cosmo IR dependency is avoided

- **WHEN** cosmo0 lowers accepted source
- **THEN** the lowering result does not require full-language IR nodes for type functions, universe types, HKT, reflection, or staged quotation

### Requirement: Command Surface

cosmo0 SHALL expose a command or API entry point that can check and compile cosmo0-targeted source files separately from full Cosmo compilation.

#### Scenario: Check command reports success

- **WHEN** a user runs the cosmo0 check entry point on a valid cosmo0 source file
- **THEN** the command exits successfully and reports no subset errors

#### Scenario: Check command reports unsupported feature

- **WHEN** a user runs the cosmo0 check entry point on a source file using a rejected full-language feature
- **THEN** the command exits unsuccessfully and reports the source location and unsupported feature

### Requirement: Package-Level Command Surface

cosmo0 SHALL expose command or API entry points capable of checking and compiling cosmo0-targeted packages in addition to individual source files.

#### Scenario: Package check succeeds

- **WHEN** a user runs the cosmo0 package check entry point on a valid multi-file cosmo0 package
- **THEN** cosmo0 resolves package metadata, checks all modules, and exits successfully

#### Scenario: Package compile emits outputs

- **WHEN** a user runs the cosmo0 package compile entry point on a valid cosmo0 package
- **THEN** cosmo0 emits deterministic C++ outputs and runtime descriptor includes for the package

### Requirement: Multi-Module Output

cosmo0 SHALL support deterministic C++ output for multiple modules with stable namespaces, names, includes, and runtime descriptor linkage.

#### Scenario: Stable names are emitted

- **WHEN** cosmo0 compiles the same valid package twice without source changes
- **THEN** generated namespaces, symbols, and output ordering are stable across runs

#### Scenario: Runtime descriptors are included

- **WHEN** source uses a registered core0 standard type or runtime module
- **THEN** cosmo0 emits the required runtime includes or generated support code exactly once per output unit as required by the backend strategy

### Requirement: Bootstrap Smoke Target

cosmo0 SHALL include at least one bootstrap-focused smoke target that exercises standard generics, classes, variants, JSON-facing logic, and C++ emission.

#### Scenario: Parser-style target compiles

- **WHEN** the smoke target uses valid cosmo0 constructs and registered standard generic types
- **THEN** cosmo0 compiles it to C++ without relying on full Cosmo standard-library source compilation

#### Scenario: Smoke target rejects accidental full feature

- **WHEN** the smoke target introduces a user-defined generic or type-level feature
- **THEN** cosmo0 rejects the target during subset checking

### Requirement: cosmo1 Stage Validation

cosmo0 SHALL provide validation targets for staged cosmo1 compiler components.

#### Scenario: Early cosmo1 stage checks

- **WHEN** a cosmo1 stage source package uses only the accepted cosmo0 subset and registered core0 descriptors
- **THEN** cosmo0 check-package accepts the stage

#### Scenario: Early cosmo1 stage compiles

- **WHEN** a cosmo1 stage source package passes package checking and only requires implemented runtime descriptors
- **THEN** cosmo0 compile-package emits C++ for the stage

#### Scenario: Accidental full feature is rejected in cosmo1 stage

- **WHEN** a cosmo1 stage source introduces user-defined generics, host `Type`, reflection, staging, or unsupported higher-order code
- **THEN** cosmo0 rejects the stage during subset checking

### Requirement: Minimal Type Checker for Compiler Code

cosmo0 SHALL type-check the ordinary compiler-code constructs needed by cosmo1, including locals, assignments, field access, method calls, function calls, returns, variant construction, match branches, aliases, and standard descriptor substitution.

#### Scenario: Descriptor substitution type-checks

- **WHEN** source calls `arena.alloc(expr)` on `Arena<Expr>`
- **THEN** cosmo0 substitutes the descriptor type parameter and assigns the result type `Id<Expr>`

#### Scenario: Return mismatch is rejected

- **WHEN** a function returns a value whose type does not match the declared return type after alias expansion
- **THEN** cosmo0 rejects the function with a type diagnostic

### Requirement: Full Cosmo Behavior Is Unchanged

Adding cosmo0 SHALL NOT change accepted behavior for the existing full Cosmo compiler path.

#### Scenario: Existing full compiler still handles full-language source

- **WHEN** a source file is compiled through the existing full Cosmo compiler path
- **THEN** full Cosmo behavior remains governed by the existing compiler rather than cosmo0 subset restrictions

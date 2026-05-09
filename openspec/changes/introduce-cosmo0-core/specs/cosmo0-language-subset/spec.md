## ADDED Requirements

### Requirement: Shared Parser With Subset Checking

cosmo0 SHALL parse source using the Cosmo syntax accepted by the shared parser and MUST run a cosmo0 subset checker before lowering or code generation.

#### Scenario: Valid core source proceeds to lowering

- **WHEN** source uses only cosmo0-supported declarations, expressions, control flow, and standard generic type applications
- **THEN** cosmo0 accepts the parsed syntax tree and proceeds to lowering

#### Scenario: Full-language syntax is parsed but rejected

- **WHEN** source is valid full Cosmo syntax but contains a construct outside the cosmo0 subset
- **THEN** cosmo0 rejects the source before lowering with a diagnostic naming the unsupported construct

### Requirement: Supported Core Declarations

cosmo0 SHALL support non-generic `class`, enum-style `case` variants, `def`, `val`, `var`, simple `type` aliases, and imports needed to reference core0 standard capabilities.

#### Scenario: Non-generic class is accepted

- **WHEN** source defines a class with concrete fields, methods, and case variants
- **THEN** cosmo0 accepts the declaration if all referenced types are valid cosmo0 types

#### Scenario: Simple type alias is accepted

- **WHEN** source defines `type N = Ptr<Node>` or another alias over valid cosmo0 types
- **THEN** cosmo0 records the alias as a type synonym and does not treat it as a type-level value

### Requirement: Module and Package Subset

cosmo0 SHALL support enough module, package, import, and visibility behavior to check and compile multi-file bootstrap packages.

#### Scenario: Acyclic package imports are accepted

- **WHEN** a package contains multiple cosmo0 source files with acyclic imports and public declarations
- **THEN** cosmo0 resolves the imports, orders the modules, and makes public declarations available to dependent modules

#### Scenario: Import cycle is diagnosed

- **WHEN** a package contains a module dependency cycle that cosmo0 cannot compile
- **THEN** cosmo0 rejects the package with a diagnostic naming the cycle

### Requirement: Variant Data and Matching

cosmo0 SHALL support enum-style case variants, variant construction, payload fields, tag checks, and match destructuring over variants and primitive values.

#### Scenario: Variant payload is matched

- **WHEN** source constructs a variant with payload fields and later matches that variant
- **THEN** cosmo0 type-checks the bound payload fields and lowers the match into cosmo0 IR

#### Scenario: Wildcard branch handles remaining cases

- **WHEN** a match expression includes a wildcard branch
- **THEN** cosmo0 accepts the match without requiring first-stage exhaustiveness checking

### Requirement: Reference and Mutation Subset

cosmo0 SHALL support references and mutation forms required by compiler implementation code, including `&T`, `&mut T`, `&self`, `&mut self`, mutable locals, mutable fields, and mutable container access through descriptor-defined APIs.

#### Scenario: Mutable method updates state

- **WHEN** a method declared with `&mut self` updates a mutable field or pushes into a mutable vector field
- **THEN** cosmo0 accepts and lowers the mutation if the receiver and field types allow it

#### Scenario: Immutable reference rejects mutation

- **WHEN** source attempts to mutate through an immutable reference
- **THEN** cosmo0 rejects the operation with a mutability diagnostic

### Requirement: Restricted For-In

cosmo0 SHALL support `for` iteration only over descriptor-defined iterable standard types needed by compiler code.

#### Scenario: Vec for-in is accepted

- **WHEN** source iterates with `for (item in items)` where `items` has type `Vec<T>`
- **THEN** cosmo0 type-checks `item` as `T` and lowers the loop using the `Vec<T>` descriptor

#### Scenario: Unsupported for-in target is rejected

- **WHEN** source iterates over a type without a registered cosmo0 iteration descriptor
- **THEN** cosmo0 rejects the loop with a diagnostic naming the unsupported iteration target

### Requirement: Supported Core Expressions

cosmo0 SHALL support literals, local variables, assignment, calls, field selection, blocks, `if`/`else`, loops, `return`, and match expressions over supported enum-style variants and primitive values.

#### Scenario: Core control flow is accepted

- **WHEN** a method body uses local variables, calls, `if`/`else`, loops, and `return`
- **THEN** cosmo0 lowers the body to the cosmo0 IR if every expression is type-correct in the subset

#### Scenario: Unsupported expression is rejected

- **WHEN** a method body uses quotation, staging, macro expansion, reflection, or type-level pattern matching
- **THEN** cosmo0 rejects the expression before lowering

### Requirement: User-Defined Generic Programming Is Rejected

cosmo0 MUST reject user-defined generic classes, generic functions, generic traits, generic impls, and explicit compile-time type parameters.

#### Scenario: Generic class is rejected

- **WHEN** source declares `class Arena[T]` or `class Arena(T: Type)`
- **THEN** cosmo0 rejects the declaration with a diagnostic that user-defined generic classes are unsupported

#### Scenario: Generic function is rejected

- **WHEN** source declares `def id[T](x: T): T` or `def id(T: Type)(x: T): T`
- **THEN** cosmo0 rejects the declaration with a diagnostic that user-defined generic functions are unsupported

### Requirement: Type-Level Cosmo Features Are Rejected

cosmo0 MUST reject `Type` as a value-level or type-level programming facility, type functions, dependent type expressions, trait constraints, HKT, reflection, staging, macros, quotation, and paste-style code generation.

#### Scenario: Type-level value is rejected

- **WHEN** source evaluates `Type(expr)` or defines a function returning `Type`
- **THEN** cosmo0 rejects the source with a diagnostic that type-level programming is outside the subset

#### Scenario: Reflection and staging are rejected

- **WHEN** source uses reflection metadata, quoted expressions, or staged paste operations
- **THEN** cosmo0 rejects the source with a diagnostic identifying the unsupported feature

### Requirement: Initial Higher-Order Features Are Rejected

cosmo0 MUST reject closures, lambdas, generic higher-order collection methods, and parser-combinator-style APIs in the initial subset.

#### Scenario: Lambda is rejected

- **WHEN** source uses a lambda expression or passes anonymous code as a value
- **THEN** cosmo0 rejects the expression with a diagnostic that higher-order functions are outside the initial subset

#### Scenario: Generic collection algorithm is rejected

- **WHEN** source calls a generic higher-order method such as `map`, `filter`, or `fold` requiring a function argument
- **THEN** cosmo0 rejects the call unless a future descriptor explicitly admits that method

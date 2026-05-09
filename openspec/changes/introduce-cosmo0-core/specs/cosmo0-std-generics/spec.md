## ADDED Requirements

### Requirement: Sealed Standard Generic Constructors

cosmo0 SHALL support generic type application only for a closed set of registered core0 standard constructors.

#### Scenario: Registered generic type is accepted

- **WHEN** source uses `Vec<Node>`, `Option<Node>`, `Result<Node, Error>`, `Map<Symbol, DefId>`, `Set<Symbol>`, `Arena<Expr>`, `Id<Expr>`, `Ptr<Node>`, or another registered standard generic constructor with the correct arity
- **THEN** cosmo0 accepts the type application and records it as a cosmo0 type

#### Scenario: Unknown generic type is rejected

- **WHEN** source uses a generic constructor that is not registered as a core0 standard constructor
- **THEN** cosmo0 rejects the type application with a diagnostic naming the unknown constructor

### Requirement: Nested Standard Generic Types

cosmo0 SHALL allow nested applications of registered standard generic constructors.

#### Scenario: Nested vector option type is accepted

- **WHEN** source references a type such as `Option<Vec<Ptr<Node>>>`
- **THEN** cosmo0 accepts the type if every constructor is registered and every type argument is valid

#### Scenario: Nested invalid constructor is rejected

- **WHEN** source references a type such as `Option<UserGeneric<Node>>`
- **THEN** cosmo0 rejects the complete type because `UserGeneric` is not a registered standard constructor

### Requirement: Standard Implementations Are Forced

cosmo0 MUST lower registered standard generic types and their methods using the implementation described by core0 standard descriptors, independent of user source declarations.

#### Scenario: Vec lowers to the registered implementation

- **WHEN** source declares a value of type `Vec<Node>` and calls supported `Vec` methods
- **THEN** cosmo0 emits C++ using the registered `Vec` descriptor implementation and method lowering

#### Scenario: User source cannot override Vec

- **WHEN** source declares a local class, type alias, or import named `Vec`
- **THEN** cosmo0 does not allow that declaration to replace the registered core0 `Vec` constructor

### Requirement: Descriptor-Defined Method Surface

cosmo0 SHALL type-check method calls on standard generic instances against descriptor-defined method signatures.

#### Scenario: Vec push type-checks with element type

- **WHEN** source calls `nodes.push(node)` where `nodes` has type `Vec<Node>` and `node` has type `Node`
- **THEN** cosmo0 accepts the call and assigns it the descriptor-defined return type

#### Scenario: Vec push rejects wrong element type

- **WHEN** source calls `nodes.push(name)` where `nodes` has type `Vec<Node>` and `name` has type `String`
- **THEN** cosmo0 rejects the call with a type mismatch diagnostic

### Requirement: Compiler-Oriented Container APIs

cosmo0 SHALL provide descriptor-defined APIs for compiler-oriented standard containers including `Vec<T>`, `Map<K, V>`, `Set<T>`, `Arena<T>`, and `Id<T>`.

#### Scenario: Arena alloc returns typed id

- **WHEN** source calls `exprs.alloc(expr)` where `exprs` has type `Arena<Expr>` and `expr` has type `Expr`
- **THEN** cosmo0 assigns the result type `Id<Expr>`

#### Scenario: Arena get requires matching id type

- **WHEN** source calls `exprs.get(id)` where `exprs` has type `Arena<Expr>`
- **THEN** cosmo0 accepts the call only when `id` has type `Id<Expr>`

#### Scenario: Map get returns optional reference

- **WHEN** source calls `defs.get(symbol)` where `defs` has type `Map<Symbol, DefId>`
- **THEN** cosmo0 assigns a descriptor-defined optional value or optional reference result type for `DefId`

### Requirement: Restricted Map Key Types

cosmo0 SHALL restrict `Map<K, V>` and `Set<K>` key types to descriptor-supported key types until trait-based hashing or ordering exists.

#### Scenario: Supported key type is accepted

- **WHEN** source declares `Map<Symbol, DefId>`, `Map<String, TyId>`, `Map<usize, ExprId>`, or `Map<Id<Scope>, Vec<DefId>>`
- **THEN** cosmo0 accepts the type if the key is registered as a supported map key type

#### Scenario: Unsupported key type is rejected

- **WHEN** source declares `Map<Expr, DefId>` and `Expr` is not a registered key type
- **THEN** cosmo0 rejects the map type with a diagnostic naming the unsupported key type

### Requirement: Deterministic Collection Behavior

cosmo0 SHALL provide deterministic iteration behavior for output-affecting standard collections or require users to choose an explicitly deterministic collection.

#### Scenario: Deterministic map iteration is available

- **WHEN** cosmo1 code iterates over definition, scope, package, or artifact maps that affect diagnostics, snapshots, or generated C++
- **THEN** cosmo0 provides a deterministic iteration path

#### Scenario: Non-deterministic iteration is not used implicitly

- **WHEN** a collection descriptor has non-deterministic iteration semantics
- **THEN** cosmo0 does not use that iteration for output-affecting compiler state unless the source explicitly opts into it

### Requirement: Generic Types Are Not First-Class Values

cosmo0 MUST NOT treat standard generic constructors or instantiated generic types as first-class runtime values.

#### Scenario: Constructor target is accepted

- **WHEN** source constructs `Vec<Node>()` or the equivalent canonical cosmo0 constructor syntax
- **THEN** cosmo0 treats the type application as a constructor target, not as a runtime value

#### Scenario: Generic type value is rejected

- **WHEN** source assigns `Vec<Node>` to a variable or passes it as a runtime argument
- **THEN** cosmo0 rejects the expression because generic types are not first-class values

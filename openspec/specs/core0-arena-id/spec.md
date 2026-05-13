# core0-arena-id Specification

## Purpose
TBD - created by archiving change add-core0-arena-id. Update Purpose after archive.
## Requirements
### Requirement: Minimal Arena And Id Capability

cosmo0 SHALL expose the `core0.arena-id` standard capability for sealed `Arena[T]` storage and phantom-typed `Id[T]` handles.

#### Scenario: Arena allocation returns typed ids

- **WHEN** source allocates an `Expr` into an `Arena[Expr]`
- **THEN** cosmo0 assigns the result type `Id[Expr]`
- **AND** the ID is not assignable to `Id[Ty]` or any other arena item type

#### Scenario: Arena lookup requires matching ids

- **WHEN** source looks up a value from `Arena[Expr]`
- **THEN** `get` accepts only an `Id[Expr]`
- **AND** returns an immutable reference to `Expr`
- **AND** `get_mut` accepts only an `Id[Expr]` through a mutable arena receiver
- **AND** returns a mutable reference to `Expr`

#### Scenario: Arena length is available

- **WHEN** source calls `len` or `size` on an `Arena[Expr]`
- **THEN** cosmo0 assigns the result type `usize`
- **AND** the value reflects the number of allocated items in the arena

### Requirement: Arena And Id Are Sealed Standard Applications

`Arena[T]` and `Id[T]` SHALL be sealed registered standard type applications, not user-defined generic declarations.

#### Scenario: Registered applications are accepted

- **WHEN** source declares `Arena[Expr]`, `Id[Expr]`, or an alias such as `type ExprId = Id[Expr]`
- **THEN** cosmo0 accepts the application with exactly one type argument

#### Scenario: User-defined generic arena declarations remain rejected

- **WHEN** source declares `class Arena[T]` or attempts to introduce a user-defined generic ID helper
- **THEN** cosmo0 rejects the declaration before lowering

### Requirement: Syntax AST Arena Usage

cosmo1 syntax AST source SHALL be able to store syntax nodes in arenas and use aliases for typed syntax IDs.

#### Scenario: Syntax nodes are allocated and retrieved through typed IDs

- **WHEN** cosmo1 syntax AST source allocates an expression node into an `Arena[SyntaxExpr]`
- **THEN** the returned `ExprId` can be used to retrieve that expression
- **AND** IDs for other syntax arena item types cannot be mixed with expression IDs


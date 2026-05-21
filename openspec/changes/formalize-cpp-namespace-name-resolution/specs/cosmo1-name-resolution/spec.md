## MODIFIED Requirements

### Requirement: Symbol And Definition Resolution

cosmo1 SHALL resolve parser-source declarations, names, fields, methods, parameters, locals, supported qualified paths, and explicit foreign namespace aliases to stable identifiers before type checking.

#### Scenario: Parser names resolve

- **WHEN** cosmo1 resolves the supported `parser.cos` source subset
- **THEN** top-level functions, classes, methods, fields, parameters, locals, and `self` references resolve to stable definition identifiers

#### Scenario: Invalid name is diagnosed

- **WHEN** a parser-source reference cannot be resolved
- **THEN** cosmo1 reports an unresolved-name diagnostic with the reference span

#### Scenario: Explicit C++ namespace alias resolves as qualified root

- **WHEN** a source file imports `std as cstd from "c++/vector"` and references `cstd::vector`
- **THEN** cosmo1 resolves the leftmost segment `cstd` to a stable foreign namespace alias identifier
- **AND** records the remaining qualified suffix `vector` for later foreign symbol validation

## ADDED Requirements

### Requirement: Formal Name Resolution Algorithm

cosmo1 SHALL define and implement name resolution as a deterministic phase with documented inputs, outputs, phase order, lookup rules, and diagnostics.

#### Scenario: Resolver phases are documented

- **WHEN** a developer reads the name-resolution documentation
- **THEN** the document describes declaration collection, import classification, duplicate/conflict validation, scope construction, unqualified lookup, qualified lookup, and diagnostic emission in order
- **AND** the document names the resolver inputs and outputs consumed by type checking and lowering

#### Scenario: Unqualified lookup is lexical and deterministic

- **WHEN** an unqualified name is resolved inside nested lexical scopes
- **THEN** cosmo1 searches from the innermost scope outward
- **AND** returns the nearest non-conflicting binding
- **AND** reports an unresolved-name diagnostic when no binding is found

#### Scenario: Qualified lookup resolves the leftmost segment first

- **WHEN** cosmo1 resolves a qualified path
- **THEN** it resolves the leftmost segment using ordinary lexical lookup
- **AND** resolves each subsequent segment according to the binding kind produced by the previous segment
- **AND** reports an unresolved-name or invalid-qualified-path diagnostic when a segment cannot be resolved for that binding kind

#### Scenario: Conflicts are diagnosed before reference lookup

- **WHEN** two declarations or imports introduce incompatible bindings into the same scope
- **THEN** cosmo1 reports duplicate or conflicting binding diagnostics before resolving references that could depend on those bindings

### Requirement: Name Resolution Fixtures

cosmo1 SHALL include name-resolution fixture files that exercise ordinary Cosmo lookup, qualified lookup, duplicate diagnostics, unresolved-name diagnostics, and C++ namespace alias behavior.

#### Scenario: Fixture corpus covers positive and negative resolver cases

- **WHEN** the name-resolution fixture tests are run
- **THEN** fixtures under `fixtures/name-resolution` cover lexical local lookup, top-level lookup, qualified member or module lookup, duplicate binding diagnostics, unresolved-name diagnostics, compatible C++ namespace alias merging, incompatible C++ namespace alias conflicts, and unsupported `import "c++/..."` diagnostics

#### Scenario: Fixture diagnostics use stable codes

- **WHEN** a negative name-resolution fixture expects a diagnostic
- **THEN** the fixture records the stable diagnostic code and source span expected from the resolver phase

## ADDED Requirements

### Requirement: Symbol And Definition Resolution

cosmo1 SHALL resolve parser-source declarations, names, fields, methods, parameters, locals, and supported qualified paths to stable identifiers before type checking.

#### Scenario: Parser names resolve

- **WHEN** cosmo1 resolves the supported `parser.cos` source subset
- **THEN** top-level functions, classes, methods, fields, parameters, locals, and `self` references resolve to stable definition identifiers

#### Scenario: Invalid name is diagnosed

- **WHEN** a parser-source reference cannot be resolved
- **THEN** cosmo1 reports an unresolved-name diagnostic with the reference span

### Requirement: Duplicate Definitions

cosmo1 SHALL reject duplicate definitions in the same scope.

#### Scenario: Duplicate name is rejected

- **WHEN** two definitions introduce the same name into one scope
- **THEN** cosmo1 reports a duplicate-definition diagnostic before type checking

## ADDED Requirements

### Requirement: Declaration Signatures

cosmo1 SHALL resolve parser-source declaration signatures before expression body type checking.

#### Scenario: Parser declarations resolve

- **WHEN** cosmo1 resolves declarations from the supported `parser.cos` subset
- **THEN** it records top-level function, class, field, method, value, alias, parameter, receiver, and return-type signatures

#### Scenario: Function bodies are deferred

- **WHEN** declaration signatures are resolved
- **THEN** function and method bodies do not need to be type checked by this capability

### Requirement: Type Expression Resolution

cosmo1 SHALL resolve primitive, user, alias, function, immutable reference, and mutable reference type expressions needed by `parser.cos`.

#### Scenario: Unknown type is rejected

- **WHEN** a type annotation names an unknown type
- **THEN** cosmo1 reports an unknown-type diagnostic with the type expression span

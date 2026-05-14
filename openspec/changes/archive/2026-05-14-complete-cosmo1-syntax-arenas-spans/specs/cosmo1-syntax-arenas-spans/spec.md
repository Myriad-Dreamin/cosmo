## ADDED Requirements

### Requirement: Arena Backed Syntax

cosmo1 SHALL store parser self-compile syntax in arenas with stable identifiers for declarations, class members, parameters, type expressions, and expressions.

#### Scenario: Syntax ids are stable

- **WHEN** the same supported `parser.cos` source is parsed repeatedly
- **THEN** equivalent syntax nodes receive deterministic traversal order and stable debug output

#### Scenario: Syntax references use ids

- **WHEN** a syntax node refers to another syntax node
- **THEN** the reference uses an arena-backed syntax identifier rather than a raw pointer

### Requirement: Syntax Spans

cosmo1 SHALL attach source spans to syntax nodes used by parser self-compile diagnostics.

#### Scenario: Diagnostic node has span

- **WHEN** resolution or type checking reports a diagnostic for a parser-source syntax node
- **THEN** the diagnostic can identify the source span carried by that node

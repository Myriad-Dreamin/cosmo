## ADDED Requirements

### Requirement: Native Parser Produces Syntax

cosmo1 SHALL parse the supported `packages/cosmoc/src/parser.cos` subset into structured syntax nodes rather than only returning a Boolean accepted or rejected result.

#### Scenario: Parser source produces declarations

- **WHEN** cosmo1 parses the supported `parser.cos` source subset
- **THEN** the parser output contains syntax nodes for top-level classes, functions, type aliases, values, class members, method parameters, type annotations, and method bodies

#### Scenario: Parser output covers required expressions

- **WHEN** parser output includes function and method bodies
- **THEN** it represents blocks, locals, assignments, calls, selections, binary and unary operations, `if`, `while`, and `return` nodes used by `parser.cos`

#### Scenario: Unsupported syntax remains out of scope

- **WHEN** source uses full-language features outside the `parser.cos` subset
- **THEN** this parser slice may reject the source or produce explicit unsupported syntax diagnostics without adding full-language support

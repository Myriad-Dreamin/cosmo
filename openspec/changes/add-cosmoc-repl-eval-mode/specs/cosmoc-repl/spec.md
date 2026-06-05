## ADDED Requirements

### Requirement: cosmoc REPL Mode

cosmoc SHALL provide a REPL mode with an explicit session lifecycle for
incremental declarations and evaluated inputs.

#### Scenario: REPL mode starts

- **WHEN** the user starts cosmoc in REPL mode
- **THEN** cosmoc creates a REPL session with a stable session id, empty accepted
  source history, default imports/configuration, resource limits, and eval cache
  state
- **AND** the session does not depend on cosmo0 eval

#### Scenario: REPL mode exits

- **WHEN** the user exits the REPL
- **THEN** cosmoc disposes session-owned eval artifacts and reports or clears
  cache state according to the selected driver policy

### Requirement: REPL Input Classification

cosmoc SHALL classify each submitted REPL input before mutating accepted session
state.

#### Scenario: Declaration input is accepted

- **WHEN** a submitted input is a supported declaration or import
- **THEN** cosmoc checks it against the current REPL session
- **AND** accepted declarations or imports become session facts visible to later
  entries

#### Scenario: Failed input rolls back

- **WHEN** a submitted input fails parsing, checking, or unsupported-form
  validation
- **THEN** cosmoc reports deterministic diagnostics
- **AND** the failed input does not mutate accepted session facts

### Requirement: REPL Evaluation Uses cosmoc Eval

cosmoc SHALL evaluate accepted REPL expression inputs through cosmoc eval using
ordinary Clang provider-entry compilation and PCH/precompiled context reuse.

#### Scenario: Expression input is evaluated

- **WHEN** a submitted input is accepted as an evaluable expression
- **THEN** cosmoc lowers it into a `CosmoEvalRequest` containing REPL session
  identity, accepted declarations/imports, generated provider-entry source,
  target settings, resource limits, precompiled context key, compile options,
  and toolchain identity
- **AND** cosmoc eval compiles and invokes the provider entry through ordinary
  Clang compilation
- **AND** cosmoc renders the structured `CosmoEvalResult`

#### Scenario: clang interpreter path is rejected

- **WHEN** REPL evaluation needs C++ execution
- **THEN** cosmoc does not use clang-repl or clangInterpreter as the execution
  substrate
- **AND** it does not call cosmo0 eval

### Requirement: REPL Cache And Bounds

cosmoc REPL SHALL reuse compatible PCH/precompiled contexts and enforce
resource bounds for interactive evaluation.

#### Scenario: Compatible entries reuse cache

- **WHEN** two REPL eval requests have the same precompiled context key
- **THEN** cosmoc eval may reuse the cached PCH/precompiled context
- **AND** the rendered result can include a cache reuse summary

#### Scenario: Resource bound is exceeded

- **WHEN** REPL evaluation exceeds a configured compile or execution bound
- **THEN** cosmoc reports a stable bound diagnostic
- **AND** the session remains usable for later inputs

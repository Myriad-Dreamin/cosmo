## ADDED Requirements

### Requirement: Gated Expression Macro Calls

cosmo0 SHALL provide a gated expression macro-call implementation slice that is
disabled by default until an accepted profile enables it.

#### Scenario: Macro calls are disabled by default

- **WHEN** normal cosmo0 checking encounters source that would be an expression
  macro call in the gated profile
- **THEN** cosmo0 preserves existing ordinary or unsupported behavior
- **AND** it does not invoke a macro provider

#### Scenario: Enabled profile invokes expression macro

- **WHEN** the expression macro-call profile is enabled
- **AND** an accepted call site resolves to a registered macro provider
- **THEN** cosmo0 invokes the provider through the macro function input/output
  boundary

### Requirement: Resolved-Target Macro Classification

cosmo0 SHALL classify expression macro calls only after ordinary resolution
selects a macro provider target.

#### Scenario: Free call resolves to macro provider

- **WHEN** `vec(1, 2, 3)` is parsed
- **AND** ordinary callee resolution selects `@macro def vec(...)`
- **THEN** cosmo0 classifies the expression as a macro invocation

#### Scenario: Method-like selector resolves to macro provider

- **WHEN** `value.expand(2)` is parsed
- **AND** receiver type facts make member lookup runnable
- **AND** method-set facts are available when trait or extension lookup can
  contribute `.expand` candidates
- **AND** that lookup resolves `.expand` to a macro provider
- **THEN** cosmo0 classifies the expression as a macro invocation

#### Scenario: Derive-generated impl contributes method-like macro provider

- **WHEN** a derive-generated implementation contributes a trait or extension
  method named `.expand` for the receiver type
- **THEN** method-like macro classification waits for the corresponding
  method-set fact
- **AND** the selector is classified only after that fact resolves `.expand` to
  a macro provider

#### Scenario: Free macro does not match selector by text

- **WHEN** a free `@macro def expand(...)` is in scope
- **AND** `.expand` lookup on `value` does not resolve to that provider
- **THEN** `value.expand(2)` is not classified as a macro by string name alone

### Requirement: Single Normalized Payload

Each expression macro invocation SHALL provide exactly one normalized
`Expr[Untyped]` payload to the provider.

#### Scenario: Parenthesized arguments produce Args

- **WHEN** an accepted parenthesized call site selects a macro provider
- **THEN** the provider receives one `Expr.Args` payload containing positional
  and named arguments

#### Scenario: Method-like syntax preserves receiver

- **WHEN** an accepted method-like call site selects a macro provider
- **THEN** the provider receives one `Expr.Args` payload with the receiver
  preserved as an ordinary expression field

#### Scenario: Attached block produces Block

- **WHEN** an accepted block-attached site selects a macro provider
- **THEN** the provider receives one `Expr.Block` payload

#### Scenario: Template produces Template

- **WHEN** an accepted template or interpolation site selects a macro provider
- **THEN** the provider receives one `Expr.Template` payload containing
  structured literal parts and expression holes

#### Scenario: Multiple surface forms are not multiple provider arguments

- **WHEN** source uses a form such as `A(1) { block }`
- **THEN** cosmo0 does not pass two provider arguments for one macro invocation
- **AND** the form is either rejected by the macro-call profile or handled by a
  later capability

### Requirement: Provider Output Re-enters Ordinary Checking

Expression macro providers SHALL return untrusted generated `Expr[Untyped]`
output that is validated and checked by ordinary compiler phases.

#### Scenario: Generated expression is rechecked

- **WHEN** an expression macro provider returns generated `Expr[Untyped]`
  output
- **THEN** cosmo0 validates the output shape and origin metadata
- **AND** the generated expression re-enters ordinary name resolution and type
  checking

#### Scenario: Typed expression injection is rejected

- **WHEN** a provider attempts to return a trusted typed expression artifact or
  direct compiler mutation handle
- **THEN** cosmo0 rejects the output with a macro diagnostic
- **AND** the artifact does not enter the checked program

### Requirement: Macro Call Diagnostics Are Deterministic

cosmo0 SHALL report expression macro-call diagnostics in deterministic order.

#### Scenario: Provider is missing

- **WHEN** an enabled macro call resolves to a provider path that is not
  registered or not available in the profile
- **THEN** cosmo0 reports an unresolved or disabled macro provider diagnostic
  at the selected macro site

#### Scenario: Provider output is invalid

- **WHEN** a provider returns malformed expression output
- **THEN** cosmo0 reports an invalid macro output diagnostic
- **AND** the malformed output is not spliced into the source tree

#### Scenario: Repeated expansion is stable

- **WHEN** the same enabled macro-call fixture is checked twice with the same
  provider registry and source inputs
- **THEN** macro invocation order, provider inputs, generated expression
  summaries, diagnostics, and checked output are stable

## ADDED Requirements

### Requirement: Gated Derive Macro Implementation

cosmo0 SHALL provide a gated derive macro implementation slice for supported
item declarations.

#### Scenario: Derive profile is disabled

- **WHEN** normal cosmo0 checking encounters `@derive(path)` without the derive
  profile enabled
- **THEN** cosmo0 preserves existing unsupported decorator behavior
- **AND** it does not invoke a derive provider

#### Scenario: Derive profile is enabled

- **WHEN** the derive profile is enabled
- **AND** a supported item declaration has `@derive(path)`
- **THEN** cosmo0 resolves the derive provider and builds a stable derive input
  record for that existing item

### Requirement: Derive Output Is Trait Implementation Only

cosmo0 SHALL restrict first-slice derive provider output to implementations of
existing traits for existing target items.

#### Scenario: Provider returns trait implementation

- **WHEN** a derive provider returns an implementation record for an already
  resolved trait and the annotated target item
- **THEN** cosmo0 validates the implementation record
- **AND** attaches the implementation when validation succeeds

#### Scenario: Provider returns new declaration

- **WHEN** a derive provider returns a new top-level declaration, member, field,
  variant, alias, class, raw source text, or trusted typed expression artifact
- **THEN** cosmo0 reports an invalid derive output diagnostic
- **AND** the output does not enter later compiler phases

### Requirement: Derive Does Not Affect Name Resolution

cosmo0 derive expansion SHALL NOT add new bindings to the ordinary
name-resolution environment in the first slice.

#### Scenario: Generated trait impl does not create new name

- **WHEN** `@derive(cli.Parser)` attaches an implementation for `Config`
- **THEN** the ordinary declaration index remains unchanged
- **AND** no new top-level function, static method, field, variant, or alias
  becomes name-resolvable because of the derive output

#### Scenario: Trait API uses existing names

- **WHEN** later source uses parser behavior through a trait API such as
  `cli.Parser.parse[Config](args)`
- **THEN** `cli.Parser` and `Config` resolve through ordinary name resolution
  before derive output is attached
- **AND** the derive output supplies implementation evidence rather than a new
  binding

### Requirement: Derive Contributes Trait Implementation Facts

cosmo0 derive expansion SHALL contribute implementation facts that can be used
by trait resolution and selector method-set resolution.

#### Scenario: Generated impl fact satisfies trait use

- **WHEN** `@derive(cli.Parser)` attaches an implementation for `Config`
- **AND** later source requires `cli.Parser for Config`
- **THEN** trait resolution waits for and consumes the derive-generated
  `ImplFact(cli.Parser for Config)`
- **AND** ordinary name resolution is not rerun

#### Scenario: Method-like selector waits for method-set fact

- **WHEN** a method-like selector such as `config.parse()` can be supplied by a
  trait or extension implementation
- **THEN** selector resolution waits for `TypeFact(config)`
- **AND** it waits for the method-set fact for the receiver type and selector
  name when trait or extension lookup can contribute candidates
- **AND** derive-generated implementation facts are included in that method-set
  fact

#### Scenario: Derive input does not depend on trait resolution

- **WHEN** cosmo0 builds derive input in the first derive slice
- **THEN** the input may depend on declaration headers, selected trait identity,
  target item identity, fields, variants, attributes, defaults, doc comments,
  and admitted type facts
- **AND** it does not depend on arbitrary body checking or trait-resolution
  facts unless a later capability admits explicit inspector dependencies

### Requirement: Derive Input Uses Existing Item Facts

cosmo0 SHALL build derive input from compiler-selected facts for the annotated
item.

#### Scenario: Class metadata is supplied

- **WHEN** a derive provider is invoked for a supported class declaration
- **THEN** the input includes target identity, module path, visibility, source
  spans, admitted type parameters, fields, defaults, attributes, doc comments,
  and selected trait identity

#### Scenario: Provider cannot mutate compiler state

- **WHEN** a derive provider receives input
- **THEN** it does not receive a mutable compiler module, typed tree patch
  handle, lowering IR handle, backend output handle, or target runtime
  executable handle

### Requirement: Generated Implementation Validation

cosmo0 SHALL validate generated implementation records before attachment.

#### Scenario: Generated implementation satisfies trait

- **WHEN** a generated implementation provides the members required by the
  selected trait with accepted signatures and bodies
- **THEN** cosmo0 accepts the implementation and records generated origin spans

#### Scenario: Duplicate implementation is rejected

- **WHEN** a generated implementation conflicts with an existing implementation
  for the same trait and target item
- **THEN** cosmo0 reports a duplicate derive implementation diagnostic
- **AND** it does not silently replace the existing implementation

### Requirement: Derive Diagnostics Are Deterministic

cosmo0 SHALL report derive diagnostics and generated implementation summaries
in deterministic order.

#### Scenario: Invalid provider output is reported

- **WHEN** a derive provider returns malformed implementation output
- **THEN** cosmo0 reports an invalid derive output diagnostic at the derive
  attribute and generated output origin

#### Scenario: Repeated derive expansion is stable

- **WHEN** the same derive fixture is checked twice with the same provider
  registry and source inputs
- **THEN** provider input, provider output, consumed attributes, generated
  implementation summaries, and diagnostics are stable

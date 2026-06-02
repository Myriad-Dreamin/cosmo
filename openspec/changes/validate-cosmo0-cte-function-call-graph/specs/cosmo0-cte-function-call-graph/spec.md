## ADDED Requirements

### Requirement: Gated Compile-Time Callable Graph

cosmo0 SHALL provide a gated validation mode that builds a deterministic graph
for compile-time callable fixtures without enabling production constant
function evaluation by default.

#### Scenario: Validation mode is disabled

- **WHEN** normal cosmo0 checking encounters source that resembles a
  compile-time function call fixture
- **THEN** cosmo0 preserves the existing unsupported or ordinary checking
  behavior
- **AND** it does not route the source through the callable graph validation
  mode

#### Scenario: Callable headers are indexed before entries execute

- **WHEN** the validation mode is enabled
- **THEN** cosmo0 indexes compile-time callable headers before evaluating
  selected entry calls
- **AND** source order does not decide whether an otherwise valid helper can be
  found

### Requirement: Dependent Constant Function Calls

The validation mode SHALL support constant function call entries whose result
depends on other accepted constant functions.

#### Scenario: Helper and caller are source-order independent

- **WHEN** an enabled fixture defines a constant helper function and a constant
  caller that invokes the helper in either source order
- **THEN** cosmo0 resolves the helper call through the callable graph
- **AND** the entry result is produced through the CTE/JIT boundary
- **AND** the helper header, body, and host artifact are checked or compiled at
  most once per validation plan

### Requirement: Recursive Constant Function Calls

The validation mode SHALL classify recursive callable groups as strongly
connected components and SHALL execute supported groups as one host artifact.

#### Scenario: Direct recursion terminates within bounds

- **WHEN** an enabled fixture defines a directly recursive constant function
  with an entry input that terminates within the configured validation bounds
- **THEN** cosmo0 compiles the recursive callable SCC as one host artifact
- **AND** the entry result is reported deterministically

#### Scenario: Mutual recursion terminates within bounds

- **WHEN** an enabled fixture defines mutually recursive constant functions
  with an entry input that terminates within the configured validation bounds
- **THEN** cosmo0 compiles the mutual recursion SCC as one host artifact
- **AND** the entry result is reported deterministically

#### Scenario: Recursive execution exceeds bounds

- **WHEN** recursive compile-time execution exceeds the configured validation
  bound
- **THEN** cosmo0 reports a stable recursion-bound diagnostic
- **AND** it does not depend on host process hangs or source-order behavior

### Requirement: Callable Graph Diagnostics

cosmo0 SHALL report stable diagnostics for unsupported callable graph shapes.

#### Scenario: Call head is unresolved

- **WHEN** a compile-time callable fixture calls a name whose leading segment
  cannot be resolved
- **THEN** cosmo0 reports an unresolved compile-time call diagnostic at the call
  head span

#### Scenario: Unsupported call shape is rejected

- **WHEN** a compile-time callable fixture uses a call shape outside the
  validation profile
- **THEN** cosmo0 reports that the shape is unsupported by the callable graph
  validation mode
- **AND** it does not silently execute the call in a host interpreter

### Requirement: Callable Execution Uses CTE Boundary

Accepted callable graph entries SHALL execute through the structured
compile-time evaluation boundary.

#### Scenario: Callable artifact executes through JIT

- **WHEN** an accepted callable graph entry is evaluated in integration mode
- **THEN** cosmo0 sends a structured request containing the callable artifact,
  entry identity, argument payloads, source identity, target settings, bounds,
  and toolchain identity
- **AND** `cosmo-jit-sys` or a request-compatible adapter returns a structured
  result and diagnostics

#### Scenario: Host approximation is not used

- **WHEN** the validation mode computes a callable result
- **THEN** it does not use a Scala, JavaScript, or handwritten constant-folder
  approximation for the accepted execution path

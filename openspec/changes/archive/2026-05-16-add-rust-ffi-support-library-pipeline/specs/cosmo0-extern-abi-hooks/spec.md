## ADDED Requirements

### Requirement: Direct C Extern Support Library Metadata

Trusted direct C extern declarations SHALL be able to record a support-library id that identifies the Rust-backed support library required to satisfy the C ABI symbol. The id SHALL follow the shared support-library lowercase kebab-case convention and SHALL lower to a `support-library:<id>` backend requirement.

#### Scenario: Direct C extern records support library requirement

- **WHEN** a trusted declaration uses `@extern("c", name = "cosmo_support_smoke_add", supportLibrary = "support-smoke")`
- **THEN** elaboration preserves the `support-smoke` support-library id in extern metadata
- **AND** lowering records a backend requirement `support-library:support-smoke`

#### Scenario: Invalid support library id is rejected

- **WHEN** a trusted declaration uses an invalid support-library id such as `SupportSmoke`
- **THEN** elaboration fails with `cosmo0.elaborate.invalid-extern`
- **AND** the diagnostic names the invalid support-library id convention

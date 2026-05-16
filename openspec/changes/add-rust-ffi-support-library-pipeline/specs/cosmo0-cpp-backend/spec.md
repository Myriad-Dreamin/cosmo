## ADDED Requirements

### Requirement: Support Library Link Planning

The cosmo0 C++ backend SHALL consume `support-library:<id>` backend requirements by producing a support-library link plan. The link plan SHALL contain the expected staged artifact path for every required support library and SHALL preserve support-library requirements separately from runtime symbols, includes, and descriptors.

#### Scenario: Support library requirement becomes link argument

- **WHEN** C++ emission sees a direct C extern call requiring `support-library:support-smoke`
- **THEN** backend requirements include `support-library:support-smoke`
- **AND** the support-library link plan includes `target/cosmo/support-libraries/release/support-smoke/libcosmo_support_smoke.a`
- **AND** runtime symbol requirements remain represented separately from support-library requirements

#### Scenario: Invalid hand-built support library requirement is diagnosed

- **WHEN** a checked LIR module contains an invalid support-library requirement value
- **THEN** C++ backend emission fails with `cosmo0.support-library.invalid-id`
- **AND** no C++ output is treated as a successful backend result

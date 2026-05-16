## MODIFIED Requirements

### Requirement: Support Library Naming And Artifact Layout

The support-library pipeline SHALL define lowercase kebab-case support-library
ids, C symbol prefixes, artifact names, and a shared staged output layout.
Static support-library artifacts SHALL be staged under
`target/cosmo/support-libraries/<profile>/<support-library-id>/`.

#### Scenario: Support library id maps to artifact path

- **WHEN** the support-library id is `uri-sys`
- **THEN** the Rust library target is `cosmo_uri_sys`
- **AND** the C symbol prefix is `cosmo_uri_sys_`
- **AND** the release static artifact path is `target/cosmo/support-libraries/release/uri-sys/libcosmo_uri_sys.a`

#### Scenario: Ureq support library id maps to artifact path

- **WHEN** the support-library id is `ureq-sys`
- **THEN** the Rust library target is `cosmo_ureq_sys`
- **AND** the C symbol prefix is `cosmo_ureq_sys_`
- **AND** the release static artifact path is `target/cosmo/support-libraries/release/ureq-sys/libcosmo_ureq_sys.a`

## MODIFIED Requirements

### Requirement: Named Stage Capability Profiles

cosmo0 SHALL provide named stage capability profiles that contain primitive descriptor requirements, core0 standard capability requirements, and backend extern/runtime requirements.

#### Scenario: Stage 1 profile is registered

- **WHEN** package validation requests the `cosmo1.stage1` profile
- **THEN** the profile lists the Stage 1 primitive descriptor requirements
- **AND** the profile lists `core0.stage`, `core0.text`, `core0.text-output`, `core0.option-result-vec`, `core0.path-fs`, and `core0.char-class`
- **AND** the profile lists the extern/backend requirements needed by `core0.path-fs` source loading and deterministic text output

### Requirement: Missing Capability Diagnostics

cosmo0 SHALL diagnose missing requirements from a selected stage profile during package or profile validation.

#### Scenario: Missing std capability is diagnosed

- **WHEN** the `cosmo1.stage1` profile is validated without a required std capability such as `core0.text-output`
- **THEN** validation fails with a `cosmo0.stage.missing-capability` diagnostic naming the missing capability

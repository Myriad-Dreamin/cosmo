# core0-stage-capability-registry Specification

## Purpose
TBD - created by archiving change add-core0-stage-capability-registry. Update Purpose after archive.
## Requirements
### Requirement: Named Stage Capability Profiles

cosmo0 SHALL provide named stage capability profiles that contain primitive descriptor requirements, core0 standard capability requirements, and backend extern/runtime requirements.

#### Scenario: Stage 1 profile is registered

- **WHEN** package validation requests the `cosmo1.stage1` profile
- **THEN** the profile lists the Stage 1 primitive descriptor requirements
- **AND** the profile lists `core0.stage`, `core0.text`, `core0.text-output`, `core0.option-result-vec`, `core0.path-fs`, and `core0.char-class`
- **AND** the profile lists the extern/backend requirements needed by `core0.path-fs` source loading and deterministic text output

### Requirement: Package Metadata Selects A Stage Profile

cosmo0 package metadata SHALL allow a package to select a stage capability profile for package check validation.

#### Scenario: cosmoc Stage 1 metadata selects the profile

- **WHEN** the existing `packages/cosmoc` package manifest contains `stageProfile: "cosmo1.stage1"`
- **THEN** package check validates the package against the registered `cosmo1.stage1` capability profile

### Requirement: Missing Capability Diagnostics

cosmo0 SHALL diagnose missing requirements from a selected stage profile during package or profile validation.

#### Scenario: Missing std capability is diagnosed

- **WHEN** the `cosmo1.stage1` profile is validated without a required std capability such as `core0.text-output`
- **THEN** validation fails with a `cosmo0.stage.missing-capability` diagnostic naming the missing capability

### Requirement: Later Capabilities Do Not Block Stage 1

The `cosmo1.stage1` profile SHALL NOT require later-stage capabilities that are outside the Stage 1 bootstrap slice.

#### Scenario: Later standard capabilities are absent

- **WHEN** `cosmo1.stage1` is validated without `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, or `core0.big-number`
- **THEN** validation succeeds if all Stage 1 requirements are available


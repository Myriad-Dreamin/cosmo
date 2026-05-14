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
- **AND** the stage capability registry recognizes `core0.arena-id` as a later-stage standard capability

#### Scenario: Missing character classification capability is diagnosed

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.char-class`
- **THEN** validation fails with a `cosmo0.stage.missing-capability` diagnostic naming `core0.char-class`

#### Scenario: Later capabilities remain excluded

- **WHEN** the `cosmo1.stage1` profile is validated
- **THEN** it does not require `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, or `core0.big-number`

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

The stage capability registry SHALL distinguish Stage 1 requirements from later-stage standard capabilities.

#### Scenario: core0.json is not required by Stage 1

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.json`
- **THEN** validation succeeds without a missing `core0.json` diagnostic
- **AND** `core0.json` remains registered as a known later-stage standard capability

#### Scenario: later JSON profiles diagnose missing core0.json

- **WHEN** a later-stage profile explicitly requires `core0.json`
- **AND** the available standard capabilities do not include `core0.json`
- **THEN** validation reports `cosmo0.stage.missing-capability` for standard capability `core0.json`

#### Scenario: core0.map-set is not required by Stage 1

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.map-set`
- **THEN** validation succeeds without a missing `core0.map-set` diagnostic
- **AND** `core0.map-set` remains registered as a known later-stage standard capability

#### Scenario: later map/set profiles diagnose missing core0.map-set

- **WHEN** a later-stage profile explicitly requires `core0.map-set`
- **AND** the available standard capabilities do not include `core0.map-set`
- **THEN** validation reports `cosmo0.stage.missing-capability` for standard capability `core0.map-set`

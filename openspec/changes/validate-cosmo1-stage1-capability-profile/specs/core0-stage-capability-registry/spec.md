## MODIFIED Requirements

### Requirement: Named Stage Capability Profiles
cosmo0 SHALL provide named stage capability profiles that contain primitive descriptor requirements, core0 standard capability requirements, and backend extern/runtime requirements.

#### Scenario: Stage 1 profile is registered
- **WHEN** package validation requests the `cosmo1.stage1` profile
- **THEN** the profile lists the Stage 1 primitive descriptor requirements
- **AND** the profile lists `core0.stage`, `core0.text`, `core0.text-output`, `core0.option-result-vec`, `core0.path-fs`, and `core0.char-class`
- **AND** the profile lists the extern/backend requirements needed by `core0.path-fs` source loading and deterministic text output

#### Scenario: Missing character classification capability is diagnosed
- **WHEN** the `cosmo1.stage1` profile is validated without `core0.char-class`
- **THEN** validation fails with a `cosmo0.stage.missing-capability` diagnostic naming `core0.char-class`

#### Scenario: Later capabilities remain excluded
- **WHEN** the `cosmo1.stage1` profile is validated
- **THEN** it does not require `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, or `core0.big-number`

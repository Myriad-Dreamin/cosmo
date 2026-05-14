## MODIFIED Requirements

### Requirement: Later Capabilities Do Not Block Stage 1

The stage capability registry SHALL distinguish Stage 1 requirements from later-stage standard capabilities.

#### Scenario: core0.command is not required by Stage 1

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.command`
- **THEN** validation succeeds without a missing `core0.command` diagnostic
- **AND** `core0.command` remains registered as a known later-stage standard capability

#### Scenario: later command profiles diagnose missing core0.command

- **WHEN** a later-stage profile explicitly requires `core0.command`
- **AND** the available standard capabilities do not include `core0.command`
- **THEN** validation reports `cosmo0.stage.missing-capability` for standard capability `core0.command`

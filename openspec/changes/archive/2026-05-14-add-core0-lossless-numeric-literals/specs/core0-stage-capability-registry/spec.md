## MODIFIED Requirements

### Requirement: Later Capabilities Do Not Block Stage 1

The stage capability registry SHALL distinguish Stage 1 requirements from later-stage standard capabilities.

#### Scenario: core0.big-number is not required by Stage 1

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.big-number`
- **THEN** validation succeeds without a missing `core0.big-number` diagnostic
- **AND** `core0.big-number` remains registered as a known later-stage standard capability

#### Scenario: later numeric profiles diagnose missing core0.big-number

- **WHEN** a later-stage profile explicitly requires `core0.big-number`
- **AND** the available standard capabilities do not include `core0.big-number`
- **THEN** validation reports `cosmo0.stage.missing-capability` for standard capability `core0.big-number`

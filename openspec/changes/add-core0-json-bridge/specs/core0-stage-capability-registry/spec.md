## MODIFIED Requirements

### Requirement: Later-Stage Capabilities

The stage capability registry SHALL distinguish Stage 1 requirements from later-stage standard capabilities.

#### Scenario: core0.json is not required by Stage 1

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.json`
- **THEN** validation succeeds without a missing `core0.json` diagnostic
- **AND** `core0.json` remains registered as a known later-stage standard capability

#### Scenario: later JSON profiles diagnose missing core0.json

- **WHEN** a later-stage profile explicitly requires `core0.json`
- **AND** the available standard capabilities do not include `core0.json`
- **THEN** validation reports `cosmo0.stage.missing-capability` for standard capability `core0.json`

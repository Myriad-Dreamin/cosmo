## MODIFIED Requirements

### Requirement: Later Capabilities Do Not Block Stage 1

The stage capability registry SHALL distinguish Stage 1 requirements from later-stage standard capabilities.

#### Scenario: core0.map-set is not required by Stage 1

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.map-set`
- **THEN** validation succeeds without a missing `core0.map-set` diagnostic
- **AND** `core0.map-set` remains registered as a known later-stage standard capability

#### Scenario: later map/set profiles diagnose missing core0.map-set

- **WHEN** a later-stage profile explicitly requires `core0.map-set`
- **AND** the available standard capabilities do not include `core0.map-set`
- **THEN** validation reports `cosmo0.stage.missing-capability` for standard capability `core0.map-set`

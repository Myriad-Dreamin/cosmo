## MODIFIED Requirements

### Requirement: Stage 1 Capability Profile Placeholder
The cosmo0 docs SHALL describe the concrete Stage 1 capability profile for source loading, diagnostics, tokens, lexing, standard APIs, runtime requirements, and package validation.

#### Scenario: Stage 1 validation is cross-referenced
- **WHEN** reviewers inspect the Stage 1 documentation
- **THEN** it references the OpenSpec change `validate-cosmo1-stage1-capability-profile`

#### Scenario: Stage 1 capability ownership is clear
- **WHEN** a later proposal fills or changes a Stage 1 capability
- **THEN** the proposal can update the relevant owner files under `docs/cosmo0/`

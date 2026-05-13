# cosmo1-stage1-capability-profile Specification

## Purpose
TBD - created by archiving change validate-cosmo1-stage1-capability-profile. Update Purpose after archive.
## Requirements
### Requirement: Stage 1 Package Slice
The repository SHALL provide a concrete `cosmo1.stage1` package validation target for the first executable cosmo1 compiler slice.

#### Scenario: Stage 1 package selects the profile
- **WHEN** `packages/cosmoc/cosmo.json` is loaded as a cosmo0 package
- **THEN** the package metadata selects `stageProfile: "cosmo1.stage1"`
- **AND** the package source list includes core0 text, text output, path filesystem, character classification, source text, spans, source maps, diagnostics, tokens, lexing, and parser smoke sources

#### Scenario: Stage 1 package checks and compiles
- **WHEN** the Stage 1 package is checked and compiled through cosmo0
- **THEN** package validation succeeds
- **AND** generated backend output includes the Stage 1 source, span, source-map, diagnostic, token, lexer, text-output, path-fs, and char-class declarations

### Requirement: Stage 1 Unsupported Feature Fixtures
The Stage 1 validation target SHALL include negative fixtures for full-language features that are outside the allowed cosmo0 subset.

#### Scenario: Unsupported source features are rejected
- **WHEN** package validation checks Stage 1 negative fixtures for user generics, host `Type`, staging, closures, and unsupported higher-order APIs
- **THEN** each fixture is rejected as unsupported
- **AND** each diagnostic identifies the corresponding unsupported feature area

### Requirement: Stage 1 Output Is Deterministic
Stage 1 package compilation SHALL produce deterministic backend output for repeated equivalent inputs.

#### Scenario: Repeated package compilation is stable
- **WHEN** the Stage 1 package is compiled repeatedly without source changes
- **THEN** generated source output and backend requirement records remain stable


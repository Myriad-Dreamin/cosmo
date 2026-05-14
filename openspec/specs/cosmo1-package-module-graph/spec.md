# cosmo1-package-module-graph Specification

## Purpose
TBD - created by archiving change add-cosmo1-package-module-graph. Update Purpose after archive.
## Requirements
### Requirement: Package Source Selection

cosmo1 SHALL load package metadata and select source files needed by parser self-compile validation.

#### Scenario: Explicit source list is loaded

- **WHEN** package metadata lists source files for a cosmo1 validation slice
- **THEN** cosmo1 loads those sources in deterministic source-root-relative form

### Requirement: Module Graph Diagnostics

cosmo1 SHALL build an import graph and report missing imports or cycles before semantic checking.

#### Scenario: Import order is deterministic

- **WHEN** modules import other modules without cycles
- **THEN** cosmo1 orders dependencies before importing modules

#### Scenario: Invalid graph is rejected

- **WHEN** an import is missing or an import cycle exists
- **THEN** cosmo1 reports a module graph diagnostic and does not run name resolution for that package graph


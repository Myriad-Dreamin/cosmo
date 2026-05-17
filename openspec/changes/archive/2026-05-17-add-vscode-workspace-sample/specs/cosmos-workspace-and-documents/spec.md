## ADDED Requirements

### Requirement: Provisional Cosmo Project Structure

Cosmos workspace discovery SHALL use a provisional project structure compatible with virtual workspace roots, ordinary package manifests, and package-less single-file fallback.

#### Scenario: Virtual workspace root manifest

- **WHEN** a `cosmo.json` contains a `workspace.members` array and does not declare ordinary package fields such as `name`, `version`, or `sources`
- **THEN** the manifest is treated as a virtual workspace root
- **AND** each member path is resolved relative to that virtual root
- **AND** each member path points to an ordinary package directory with its own `cosmo.json`
- **AND** the virtual root itself is not treated as the owning package for arbitrary descendant files

#### Scenario: Ordinary package manifest

- **WHEN** a `cosmo.json` declares package fields such as `name`, `version`, `target`, `stageProfile`, `dependencies`, and `sources`
- **THEN** the manifest is treated as an ordinary package root
- **AND** source entries are resolved under the package `src/` directory

#### Scenario: Package-less single-file fallback

- **WHEN** a `.cos` file is not owned by an ordinary package `cosmo.json`
- **THEN** the workspace treats it as a package-less single file
- **AND** only standard library/prelude capabilities are assumed for that file

### Requirement: Sample Project Workspaces

The repository SHALL provide sample directories under `samples/workspaces/` for each provisional project structure.

#### Scenario: Samples root is a virtual workspace

- **WHEN** repository validation reads `samples/cosmo.json`
- **THEN** it is a virtual workspace root
- **AND** it references the sample ordinary packages that should be discoverable when opening `samples/`

#### Scenario: Three workspace shapes exist

- **WHEN** repository validation inspects `samples/workspaces/`
- **THEN** `virtual-root/` contains a virtual root manifest with ordinary package members
- **AND** `package/` contains an ordinary package manifest
- **AND** `single-file/` contains a `.cos` file and no local `cosmo.json`

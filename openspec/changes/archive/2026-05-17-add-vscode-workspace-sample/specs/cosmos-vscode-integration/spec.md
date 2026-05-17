## ADDED Requirements

### Requirement: VSCode Sample Project Workspaces Are Available

The repository SHALL provide sample project directories that users can open directly in VSCode to exercise the Cosmo extension without a `.code-workspace` file.

#### Scenario: User opens samples root

- **WHEN** a user opens `samples/` in VSCode
- **THEN** `samples/cosmo.json` is available as the virtual workspace root
- **AND** existing sample source directories remain visible for browsing
- **AND** opening any `.cos` sample activates the Cosmo extension through the ordinary language association

#### Scenario: User opens project-shape samples

- **WHEN** a user opens a folder under `samples/workspaces/`
- **THEN** `virtual-root/` demonstrates a virtual workspace root
- **AND** `package/` demonstrates an ordinary package root
- **AND** `single-file/` demonstrates package-less single-file fallback

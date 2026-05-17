## ADDED Requirements

### Requirement: VSCode Sample Workspace Is Available

The repository SHALL provide a checked-in VSCode workspace sample that gives users a stable manual entry point for exercising the Cosmo extension against existing sample sources.

#### Scenario: User opens the sample workspace

- **WHEN** a user opens the sample workspace file in VSCode
- **THEN** the workspace includes the existing sample package root
- **AND** `samples/HelloWorld/main.cos` is available as the primary smoke document
- **AND** opening the workspace is sufficient to activate the Cosmo extension for `.cos` files

#### Scenario: Workspace references are stable

- **WHEN** repository validation inspects the sample workspace file
- **THEN** every referenced folder or file exists in the repository
- **AND** generated output directories are not included as workspace folders

## ADDED Requirements

### Requirement: Active Document Diagnostics Refresh

The VSCode integration SHALL refresh diagnostics for the active Cosmo document when the user switches between open Cosmo editors.

#### Scenario: Switching files refreshes active diagnostics

- **WHEN** two Cosmo documents are open in VSCode
- **AND** the user switches the active editor from the first document to the second document
- **THEN** the integration refreshes diagnostics for the second document URI
- **AND** the diagnostics visible for the active editor correspond to the second document's current in-memory text
- **AND** saving the document is not required to trigger the refresh

#### Scenario: Save does not clear unchanged diagnostics

- **WHEN** a Cosmo document has diagnostics for its current in-memory text
- **AND** the user saves the document without changing that text
- **THEN** the diagnostics for that document remain consistent with the pre-save diagnostics
- **AND** the integration does not clear diagnostics solely because a save occurred

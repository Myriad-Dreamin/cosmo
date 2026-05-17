## ADDED Requirements

### Requirement: Diagnostic Publication Is URI-Scoped

Cosmos diagnostics publication SHALL be scoped to the target document URI so refresh, publish, and clear operations for one document cannot mutate diagnostics for a different open document.

#### Scenario: Refreshing one URI preserves another URI

- **WHEN** Cosmos has published diagnostics for document A and document B
- **AND** document A is refreshed
- **THEN** the refresh publishes diagnostics for document A's URI only
- **AND** document B's diagnostics are not cleared or replaced by document A's result

#### Scenario: Repeated unchanged refresh is stable

- **WHEN** Cosmos refreshes diagnostics for the same open document snapshot more than once
- **THEN** each refresh produces the same diagnostics JSON
- **AND** the result does not depend on whether the refresh was caused by open, change, active-editor switch, or save

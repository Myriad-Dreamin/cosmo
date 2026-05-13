# core0-char-class Specification

## Purpose
TBD - created by archiving change add-core0-char-class. Update Purpose after archive.
## Requirements
### Requirement: Stage 1 Character Classification Capability

cosmo0 SHALL expose the `core0.char-class` standard capability for lexer-oriented character classification helpers.

#### Scenario: ASCII helper functions are available

- **WHEN** Stage 1 source imports the `core0.char-class` API
- **THEN** it can classify ASCII alphabetic bytes
- **AND** it can classify ASCII digit bytes
- **AND** it can classify ASCII whitespace bytes

#### Scenario: Identifier helpers use Stage 1 ASCII rules

- **WHEN** Stage 1 lexer source scans an identifier
- **THEN** `A..Z`, `a..z`, and `_` are accepted as identifier starts
- **AND** identifier continues additionally accept `0..9`
- **AND** the lexer can call these helpers as ordinary std functions

#### Scenario: Unsupported non-ASCII bytes are not classified as source syntax

- **WHEN** a byte above `127` is passed to the Stage 1 classification helpers
- **THEN** it is reported as unsupported by `is_supported_source_byte`
- **AND** it is not classified as identifier, digit, or whitespace syntax by this capability

### Requirement: Character Classification APIs Remain Std-Owned

Character classification beyond primitive scalar backing SHALL be surfaced through `core0.char-class` standard APIs and stage capability metadata rather than new runtime descriptor families.

#### Scenario: Character helpers do not become descriptors

- **WHEN** descriptor metadata is inspected for lexer character classification support
- **THEN** `is_ascii_alpha`, `is_ascii_digit`, `is_ascii_whitespace`, `is_identifier_start`, and `is_identifier_continue` are not registered descriptor families
- **AND** implementations may only reuse already permitted primitive `Byte`, `Char`, and comparison operations behind the standard API boundary


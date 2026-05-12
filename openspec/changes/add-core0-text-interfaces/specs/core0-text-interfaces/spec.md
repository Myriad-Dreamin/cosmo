## ADDED Requirements

### Requirement: Stage 1 Text Standard API

cosmo0 SHALL expose the `core0.text` standard capability for Stage 1 source text operations over owned `String` values, byte-indexed text views, ASCII-oriented character access, and diagnostic-oriented text construction.

#### Scenario: Source reads text length and emptiness

- **WHEN** Stage 1 source imports the `core0.text` API
- **THEN** it can read the byte length of a `String`
- **AND** it can test whether the text is empty

#### Scenario: Source slices or views text

- **WHEN** Stage 1 source has byte offsets into a `String`
- **THEN** it can produce an owned slice for that byte range
- **AND** it can construct a text view that reports length, emptiness, and owned text for the same byte range

#### Scenario: Lexer reads bytes and characters

- **WHEN** Stage 1 lexing code indexes source text by byte offset
- **THEN** it can read a `Byte`
- **AND** it can read an ASCII-compatible `Char`
- **AND** broader Unicode iteration is not required by this capability

#### Scenario: Diagnostics build text incrementally

- **WHEN** Stage 1 diagnostic code needs to assemble message text
- **THEN** it can append strings and text views through a builder-like API
- **AND** it can finish the builder into an owned `String`

### Requirement: Cosmo1 Source Text Helpers

cosmo1 Stage 1 SHALL provide source/source-map helpers layered on `core0.text`.

#### Scenario: SourceText wraps owned source content

- **WHEN** cosmo1 Stage 1 code constructs source text from a name and owned contents
- **THEN** it can query length, emptiness, slices, views, byte access, character access, and prefix matches through helper methods

#### Scenario: SourceMap resolves byte offsets

- **WHEN** cosmo1 Stage 1 code constructs a source map for source text
- **THEN** it can convert a byte offset to line, column, and clamped offset information
- **AND** it can retrieve the line text containing a byte offset

### Requirement: Text APIs Remain Std-Owned

Text operations beyond primitive scalar backing SHALL be surfaced through `core0.text` standard APIs and stage capability metadata rather than new runtime descriptor families.

#### Scenario: Text capability is validated as a std capability

- **WHEN** the `cosmo1.stage1` profile is validated without `core0.text`
- **THEN** validation reports a missing std capability diagnostic for `core0.text`

#### Scenario: Text helpers do not become descriptors

- **WHEN** descriptor metadata is inspected for text helper types
- **THEN** `TextBuilder`, `TextView`, `TextSlice`, `SourceText`, and `SourceMap` are not registered descriptor families
- **AND** implementations may only reuse already permitted primitive `String` backing operations behind the standard API boundary

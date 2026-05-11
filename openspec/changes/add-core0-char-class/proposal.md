## Why

The Stage 1 lexer needs character classification helpers. These should be ordinary std functions over `Char` or `Byte`, not descriptor operations.

## What Changes

- Add lexer-oriented classification helpers such as whitespace, digit, identifier start, and identifier continue checks.
- Specify the initial ASCII or supported-character behavior.
- Add `lex/lexer.cos` scanning for identifiers, numbers, whitespace, and punctuation.
- Add tokenization tests through cosmo1 source.

## Capabilities

### New Capabilities

- `core0-char-class`: Provides Stage 1 lexer classification helpers.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds or satisfies the `core0.char-class` capability.

## Impact

- Unblocks real lexer scanning in cosmo1 Stage 1.
- Keeps character helper APIs in std.
- Makes unsupported non-ASCII behavior explicit if out of scope.

## Why

The parser library target needs an executable validation program without putting
a `main` function back into `parser.cos`. A separate `parser_test.cos` program
can exercise parser behavior against shared fixtures while keeping the backend
library target clean.

## What Changes

- Add `packages/cosmoc/src/parser_test.cos` as an executable parser test program
  with a `main` function.
- Keep `packages/cosmoc/src/parser.cos` as a parser library source consumed by
  the test program.
- Add shared parser fixtures and a fixture manifest that can be consumed by both
  `parser_test.cos` and tests for `cosmo0.Parser.scala`.
- Add validation that the parser test program compiles through the cosmo0 C++
  backend and uses the shared fixture set.

## Capabilities

### New Capabilities

- `cosmoc-parser-test-program`: Defines the parser test executable and shared
  parser fixture contract for the cosmoc parser library and Scala cosmo0 parser.

### Modified Capabilities

None.

## Impact

- Adds `packages/cosmoc/src/parser_test.cos`.
- Adds parser fixture data under the cosmo0 fixture area.
- Adds or updates cosmo0 parser tests to consume the same fixture manifest.
- Depends on the cosmo0 C++ backend being able to compile the parser library
  target first.

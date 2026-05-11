## Context

`parser.cos` should be a reusable parser library. The executable behavior that
loads fixtures and reports parser-test results belongs in `parser_test.cos`.
This keeps library compilation and program compilation as separate acceptance
targets.

## Fixture Sharing

Parser fixtures should live in a repository fixture directory rather than under
Scala test snapshots or inside `packages/cosmoc`. A manifest will list fixture
paths and the expected parser outcome. Both the Scala `cosmo0.Parser` tests and
`parser_test.cos` will load the manifest, so adding or removing parser fixtures
updates one source of truth.

The exact fixture schema can be small at first:

- fixture id
- source path
- expected status, such as `ok` or `error`
- optional expected diagnostic code for negative cases

Snapshot formatting remains an implementation detail of each test runner. The
shared contract is fixture discovery and expected pass/fail behavior.

## Program Shape

`parser_test.cos` will define `main` and will import the parser library. It
should avoid becoming a second parser implementation; it only loads fixtures,
invokes the parser entry point, compares the result with the manifest, and exits
with success or failure.

The program must use constructs supported by cosmo0 lowering and the C++ backend
at the time this proposal is implemented.

## 1. Shared Fixtures

- [x] 1.1 Define the shared parser fixture directory and manifest schema.
- [x] 1.2 Add initial positive and negative parser fixtures consumed by both validation paths.
- [x] 1.3 Add Scala cosmo0 parser tests that read the shared manifest and validate `cosmo0.Parser.scala` against every fixture.

## 2. Parser Library Integration

- [x] 2.1 Ensure `packages/cosmoc/src/parser.cos` exposes a parser library API and does not define `main`.
- [x] 2.2 Ensure the parser library API can be called from another cosmoc source file.

## 3. parser_test.cos Program

- [x] 3.1 Add `packages/cosmoc/src/parser_test.cos` with a `main` function.
- [x] 3.2 Load the shared parser fixture manifest from `parser_test.cos`.
- [x] 3.3 Invoke the parser library for each fixture and compare accepted or rejected outcomes.
- [x] 3.4 Return a failing process status and identify the fixture when an outcome does not match.

## 4. Validation

- [x] 4.1 Add compile validation proving `parser_test.cos` emits C++ accepted by the configured C++ toolchain as an executable target.
- [x] 4.2 Add execution validation proving `parser_test.cos` passes the shared fixture set.
- [x] 4.3 Prove `parser_test.cos` and Scala parser tests consume the same manifest rather than duplicated fixture lists.

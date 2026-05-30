## Why

Cosmo command-line programs should be able to declare their interface as typed
source data instead of hand-writing argument scanners. The macro system can
prove its reflection and code-generation model by implementing a practical
`@derive(cli.Parser)` library while CLI11 handles the mature option parsing
behavior underneath.

## What Changes

- Add a `cosmo.cli` library with `@derive(cli.Parser)`,
  `@derive(cli.Args)`, and `@derive(cli.Subcommand)` support.
- Use macro reflection to turn typed structs and sum types into validated CLI
  schema data and generated `parse(args)` methods.
- Wrap CLI11 behind a private support boundary so Cosmo source does not depend
  on C++ parser APIs directly.
- Support the first derived CLI surface: short/long options, bool flags,
  positionals, trailing args, typed defaults, `Option[T]`, `Vec[T]`, `Path`,
  subcommands, help/version, and structured parse errors.
- Add compile-time validation for duplicate flags, invalid field types,
  ambiguous positionals, invalid subcommand shapes, and unconsumed attributes.
- Dogfood the library by migrating `packages/cosmoc/src/main.cos` from manual
  argument scanning to a derived parser.
- Update package-run entrypoint requirements to document the already-supported
  `main(args: Vec[String])` executable shape.

## Capabilities

### New Capabilities

- `core0-derived-cli`: Defines the source-facing derived CLI parser API,
  attribute semantics, type mapping, generated methods, errors, and validation.
- `cli11-sys`: Defines the CLI11 support boundary, schema/matches exchange,
  exception handling, dependency staging, and package build integration.

### Modified Capabilities

- `cosmo-package-run-cli`: Updates runnable package entrypoint requirements to
  include a single `Vec[String]` argument form in addition to zero-argument
  `main`.

## Impact

- Depends on `introduce-cosmo0-macro-system` for derive expansion, reflection
  metadata, generated declarations, and attribute consumption.
- Adds a new Cosmo library package such as `library/cli` or `library/std/src/cli`
  depending on the final std/package boundary.
- Adds a CLI11-backed native support component or support-library wrapper.
- Package build must fetch or vendor CLI11 headers deterministically and link
  any support artifact required by the wrapper.
- `packages/cosmoc/src/main.cos` and tests can become the first end-to-end
  consumer.

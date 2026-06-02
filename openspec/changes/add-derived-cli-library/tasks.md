## 1. Library Surface

- [ ] 1.1 Add the `cosmo.cli` package/module boundary and package metadata.
- [ ] 1.2 Define `CliSchema`, `CliCommand`, `CliArg`, `CliMatches`, `CliError`, and render helpers.
- [ ] 1.3 Define accepted `@command(...)` and `@arg(...)` attributes.
- [ ] 1.4 Add docs and examples for parser, args, subcommand, options, flags, defaults, and trailing args.

## 2. Macro Provider

- [ ] 2.1 Register `cli.Parser`, `cli.Args`, and `cli.Subcommand` derive providers.
- [ ] 2.2 Generate `cli.Parser for T` implementation records for a simple parser struct.
- [ ] 2.3 Generate typed extraction and target struct construction from `CliMatches` inside the trait implementation.
- [ ] 2.4 Add compile-time diagnostics for duplicate flags, unsupported types, ambiguous positionals, invalid subcommands, and unconsumed attributes.
- [ ] 2.5 Add generated-source golden tests for representative derived CLI declarations.

## 3. CLI11 Support Boundary

- [ ] 3.1 Add deterministic CLI11 dependency staging or vendoring.
- [ ] 3.2 Add a private `cli11-sys` support component that accepts schema and argv data.
- [ ] 3.3 Catch CLI11 parse/help/version exceptions and convert them into structured Cosmo results.
- [ ] 3.4 Add package build/link integration for the support component.
- [ ] 3.5 Add support-library tests proving no raw CLI11 API is exposed to user packages.

## 4. Derived Parser Features

- [ ] 4.1 Support `Bool`, `String`, `Path`, `i32`, and `usize` value mapping.
- [ ] 4.2 Support `Option[T]`, `Vec[T]`, typed defaults, required values, and repeat values.
- [ ] 4.3 Support positional fields, trailing args, and `--` terminator behavior.
- [ ] 4.4 Support `@derive(cli.Args)` flattening.
- [ ] 4.5 Support `@derive(cli.Subcommand)` sum-type command trees.
- [ ] 4.6 Support help and version rendering through the structured result model.

## 5. Dogfood And Validation

- [ ] 5.1 Update package-run specification/tests for `main(args: Vec[String])`.
- [ ] 5.2 Migrate `packages/cosmoc/src/main.cos` to `@derive(cli.Parser)`.
- [ ] 5.3 Add fixture packages for success, missing required values, invalid values, unknown flags, subcommands, and trailing args.
- [ ] 5.4 Add CLI help/error golden tests with pinned CLI11 behavior.
- [ ] 5.5 Run `scripts/check-scala-style.sh` after Scala changes and targeted package/CLI tests after implementation.

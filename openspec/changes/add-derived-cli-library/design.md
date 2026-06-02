## Context

Cosmo already supports executable packages with `main(args: Vec[String])`, and
`cmd/cosmo` forwards trailing package-run arguments to the compiled executable.
The current `packages/cosmoc/src/main.cos` still parses arguments manually with
string comparisons. That works for a tiny command surface, but it does not scale
to nested commands, structured errors, help text, typed defaults, or future
compiler tools.

CLI11 is a good first backend because it has mature C++ option parsing behavior,
including options, flags, subcommands, help, validation, and error handling.
Cosmo should not expose CLI11 directly. The public model should be typed Cosmo
source plus derive macros; CLI11 should be replaceable by a generated
palc-style parser later.

## Goals / Non-Goals

**Goals:**

- Provide a clap-like derived struct parser for Cosmo programs.
- Use `@derive(cli.Parser)`, `@derive(cli.Args)`, and
  `@derive(cli.Subcommand)` as the user-facing API.
- Use typed Cosmo fields to infer option, flag, positional, repeat, optional,
  and subcommand behavior.
- Use CLI11 behind a private support boundary for parsing, help, version, and
  common validation behavior.
- Generate implementations of existing CLI traits, used through already
  name-resolved trait APIs such as `cli.Parser.parse[Cli](args)`.
- Keep parse errors structured and renderable without forcing immediate process
  exit.
- Validate schema mistakes at compile time where reflection metadata is enough.
- Keep a path open to a future palc-like static parser backend.

**Non-Goals:**

- Expose CLI11 headers, classes, or exceptions to Cosmo user source.
- Support arbitrary custom parser callbacks in the first slice.
- Support shell completion, man page generation, colorized help, or terminal
  width wrapping in the first slice.
- Support module mocking or dynamic runtime command registration.
- Support non-UTF-8 `argv` before Cosmo has an `OsString` or `OsPath` model.
- Require self-hosted macros before the compiler-hosted derive provider exists.

## Decisions

### Make Cosmo Schema The Stable Contract

The derive macro should build a Cosmo-owned CLI schema IR before choosing a
backend. The initial parser can serialize that schema to the CLI11 wrapper and
receive structured matches/errors back. A later backend can generate a pure
Cosmo or direct C++ parser from the same schema.

Alternative considered: generate CLI11 calls directly as the only macro output.
That is fast for one backend but makes the public derive semantics depend on
CLI11 details.

### Use CLI11 Through A Support Boundary

The CLI11 wrapper should expose a narrow C ABI or trusted extern surface that
accepts a deterministic schema representation and argument strings, then returns
structured parse results. C++ exceptions must be caught inside the wrapper and
reported as Cosmo `CliError` data.

Alternative considered: bind CLI11 classes into Cosmo directly. That would leak
C++ lifetime, template, and exception behavior into the source-facing API.

### Generate Typed Trait Implementation Code

The derive provider should generate code that maps parsed matches into the
target struct inside a generated trait implementation. The CLI backend should
not construct arbitrary Cosmo objects by reflection. For example, it can return
a match table, while generated implementation code extracts `package`,
`output`, or `subcommand` values and calls the struct constructor.

Alternative considered: have CLI11 construct the final Cosmo struct. That makes
the support library depend on Cosmo object layout and backend internals.

### Keep Validation Split

Compile-time validation should catch errors visible from reflection metadata:
duplicate flags, unsupported field types, invalid subcommand target, ambiguous
positionals, bad defaults, and unconsumed attributes. Runtime validation should
handle user input: missing required values, invalid numbers, unknown flags,
help, version, and subcommand selection.

Alternative considered: let CLI11 detect all schema errors at runtime. That
would make invalid source compile and move library mistakes into executable
startup behavior.

### Start With Explicit Type Mapping

The first library should support a small fixed mapping:

```text
Bool              -> flag
String            -> value
Path              -> value parsed through std.path_fs
i32/usize         -> numeric value
Option[T]         -> optional value
Vec[T]            -> repeated value or trailing args
@derive(Args)     -> flattened argument group when annotated
@derive(Subcommand) sum type -> subcommand tree
```

Alternative considered: require user-supplied parsers for every field. That is
too much boilerplate for the first derived parser.

### Use Typed Defaults

Defaults should prefer field initializers such as `val port: usize = 8080`.
Stringly `default_value = "8080"` can be added later when the macro can validate
that the string parses through the selected value parser.

Alternative considered: use only string defaults like many CLI parsers. That
creates parse-at-runtime behavior for what should be source-checked.

## Risks / Trade-offs

- CLI11 help formatting can change across dependency versions -> pin the CLI11
  dependency and snapshot only the stable behavior needed by tests.
- Schema serialization can become a bottleneck -> keep the schema small and plan
  a generated parser backend after the API stabilizes.
- C++ exceptions can cross ABI boundaries -> catch all CLI11 exceptions inside
  `cli11-sys` and return structured errors.
- Derived CLI can overfit `cosmoc` -> include small standalone fixture packages
  with options, flags, subcommands, defaults, and trailing args.
- Attribute names can drift from clap/palc conventions -> document the accepted
  names and reject unconsumed attributes.

## Migration Plan

1. Implement the macro system smoke provider from
   `introduce-cosmo0-macro-system`.
2. Add the Cosmo `cosmo.cli` schema, matches, and error data model.
3. Add the CLI11 support boundary and deterministic dependency staging.
4. Implement `@derive(cli.Parser)` for a small single-command struct as a
   generated `cli.Parser for T` implementation.
5. Add `Args` and `Subcommand` derives.
6. Migrate `packages/cosmoc/src/main.cos` to derived parsing.
7. Add fixture packages and CLI golden tests for help, errors, and parsed values.

## Open Questions

- Whether `cosmo.cli` should live under `library/std` as a standard capability
  or as a separate package that depends on std.
- Whether the schema exchange with `cli11-sys` should use JSON first or a
  generated C ABI struct table.
- Whether help/version should be represented as successful `CliAction` values
  or as structured `CliError` variants with conventional exit codes.

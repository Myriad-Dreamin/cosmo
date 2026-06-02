## ADDED Requirements

### Requirement: Derived Parser Surface

Cosmo SHALL provide a source-facing CLI derive API that generates
implementations of existing CLI traits from annotated declarations.

#### Scenario: Parser derive generates parser trait implementation

- **WHEN** a class is annotated with `@derive(cli.Parser)`
- **THEN** macro expansion generates an implementation of the existing `cli.Parser` trait for that class
- **AND** the implementation constructs the annotated class from parsed argument values
- **AND** no new `parse` member or top-level parse function is added to ordinary name resolution

#### Scenario: Args derive can be flattened

- **WHEN** a field annotated with `@arg(flatten)` references a type annotated with `@derive(cli.Args)`
- **THEN** the parent parser includes the child argument group in the same command level

#### Scenario: Subcommand derive builds command tree

- **WHEN** a parser field annotated with `@arg(subcommand)` references a sum type annotated with `@derive(cli.Subcommand)`
- **THEN** each variant becomes a supported subcommand branch

### Requirement: CLI Attribute Semantics

The CLI derive API SHALL accept structured `@command(...)` and `@arg(...)`
attributes that describe command metadata and argument behavior.

#### Scenario: Command metadata is recorded

- **WHEN** a parser type has `@command(name = "cosmoc", version = "0.1.0-rc1")`
- **THEN** the generated schema records the command name and version
- **AND** the parse result can report help or version output for that command

#### Scenario: Field option metadata is recorded

- **WHEN** a field has `@arg(short = "p", long = "package", value_name = "PATH")`
- **THEN** the generated schema records the short flag, long flag, display value name, and field binding

### Requirement: CLI Type Mapping

The CLI derive provider SHALL infer argument behavior from supported Cosmo field
types and SHALL reject unsupported field types during macro expansion.

#### Scenario: Supported field types map to CLI values

- **WHEN** a parser declaration contains fields of type `Bool`, `String`, `Path`, `i32`, `usize`, `Option[String]`, or `Vec[String]`
- **THEN** the generated schema maps them respectively to flags, string values, path values, numeric values, optional values, and repeat or trailing values

#### Scenario: Unsupported field type is diagnosed

- **WHEN** a parser field has a type with no CLI value parser and no accepted derive behavior
- **THEN** macro expansion reports an unsupported CLI field type diagnostic at the field span

### Requirement: Compile-Time CLI Schema Validation

The CLI derive provider SHALL validate schema mistakes that are visible from
source metadata before the package is accepted.

#### Scenario: Duplicate option names are diagnosed

- **WHEN** two fields in the same command level declare the same short or long option
- **THEN** macro expansion reports a duplicate CLI option diagnostic
- **AND** the package does not defer the duplicate option error to executable startup

#### Scenario: Ambiguous positional vector is diagnosed

- **WHEN** a `Vec[T]` positional field is followed by another positional field without an explicit trailing or delimiter rule
- **THEN** macro expansion reports an ambiguous CLI positional diagnostic

#### Scenario: Invalid subcommand target is diagnosed

- **WHEN** an `@arg(subcommand)` field references a type that is not accepted by `@derive(cli.Subcommand)`
- **THEN** macro expansion reports an invalid CLI subcommand diagnostic

### Requirement: Runtime Parse Result Model

The generated parser SHALL return structured parse results without forcing
process exit from the library by default.

#### Scenario: Valid arguments construct parser struct

- **WHEN** `cli.Parser.parse[Cli](args)` receives valid arguments for the derived schema
- **THEN** it returns `Result[Cli, CliError]::Ok` containing the typed parser struct

#### Scenario: Invalid arguments return structured error

- **WHEN** `cli.Parser.parse[Cli](args)` receives an unknown option, missing value, invalid value, or missing required argument
- **THEN** it returns `Result[Cli, CliError]::Err`
- **AND** the error includes a renderable message and intended process exit code

#### Scenario: Help and version are structured

- **WHEN** `cli.Parser.parse[Cli](args)` receives help or version arguments handled by the CLI backend
- **THEN** it returns a structured result that lets the program print the backend-rendered text and exit with the conventional successful status

### Requirement: Typed Defaults

The CLI derive provider SHALL support field initializers as typed default values
for optional CLI inputs.

#### Scenario: Field initializer becomes default

- **WHEN** a numeric option field is declared with a typed initializer such as `val port: usize = 8080`
- **THEN** the generated parser uses `8080` as the default when the option is absent
- **AND** no runtime string parsing is required for that default

#### Scenario: Required field cannot also have default

- **WHEN** a field is marked required and also has a default initializer
- **THEN** macro expansion reports an invalid CLI default diagnostic

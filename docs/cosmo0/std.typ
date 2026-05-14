= cosmo0 Standard APIs

== Status

This file owns source-facing core0 standard APIs and capability identifiers. The policy and Stage 1 placeholder are normative now. Specific API signatures are placeholders until later changes add them.

== Ownership

Standard APIs are the public surface available to cosmo0 source. A backend descriptor, lowered intrinsic, or extern binding may implement a standard API, but the public behavior belongs here and in the related type, expression, runtime, and package files.

Std-owned declarations may be extern-backed when early bootstrap cannot implement them in cosmo0 source. Such declarations remain standard APIs; the extern metadata is a trusted implementation detail owned by `runtime.typ`.

== Examples

Placeholder Stage 1 source-facing shape:

```cos
import core0.text.SourceText
import core0.result.Result

def load_source(path: Path): Result<SourceText, IoError> = {
  val text = Fs.read_to_string(path)
  SourceText.from_string(text)
}
```

Implementation detail that should not become source-facing API:

```text
descriptor String::len(%source_text) -> usize
descriptor Vec<Token>::push(%tokens, %token) -> Unit
extern cosmo0.extern.v0 "::cosmo0_runtime::println"
```

The first example is a shape for future API proposals to refine. The second example names internal descriptor-style operations that may implement standard APIs but should not be the public dependency of cosmo1 source.

== Capability Identifiers

Stable core0 capability identifiers are the source-facing availability names used by stage profiles. They are not descriptor family names.

The current Stage 1 identifiers are:

- `core0.stage`: stage validation and package-profile metadata.
- `core0.text`: string/text operations used by source, token, and diagnostic code.
- `core0.text-output`: deterministic text output through a trusted std API such as `std.io.println`.
- `core0.option-result-vec`: minimal option, result, and vector surface for early compiler data.
- `core0.path-fs`: path and source-file loading surface.
- `core0.char-class`: character classification helpers for lexing.

Later-stage identifiers include `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, and `core0.big-number`. The `cosmo1.stage1` profile does not require those later capabilities. `core0.json` is a transitional standard bridge for loading parser JSON and later metadata while native cosmo1 parsing matures. `core0.arena-id` is the standard capability for typed arena storage and phantom typed IDs used by later cosmo1 syntax, type, name, and IR data.

Capability identifiers should be named at the smallest useful boundary so Stage 1 can depend on a narrow set without inheriting later compiler features.

== Extern-Backed APIs

The initial trusted extern-backed std surface is intentionally small. `core0.text-output` exposes `TextWriter` plus stdout helpers over `print(value: String): Unit` and `println(value: String): Unit`; these may lower to the C++ runtime symbols `::cosmo0_runtime::print` and `::cosmo0_runtime::println` through `cosmo0.extern.v0`. This proves the API path for deterministic smoke output without adding filesystem or command execution to the first extern smoke.

`core0.path-fs` file reading may lower to the C++ runtime symbol `::cosmo0_runtime::read_file` through `cosmo0.extern.v0`. The source-facing API remains `Fs.read_to_string(path): Result[String, IoError]`; the runtime symbol is an implementation detail.

Additional extern-backed std APIs require accepted capability text in this file plus matching runtime binding rules in `runtime.typ`. Command execution, JSON bridges, and other host-backed facilities remain std-owned API areas; they SHALL NOT be added as descriptor families merely because their implementation needs runtime support.

== Stage 1 Capability Profile

The named Stage 1 profile is `cosmo1.stage1`. It requires `core0.stage`, `core0.text`, `core0.text-output`, `core0.option-result-vec`, `core0.path-fs`, and `core0.char-class`.

The validation plan for Stage 1 is tracked by the OpenSpec change `validate-cosmo1-stage1-capability-profile`. Proposals that fill API signatures under these capability names must update this file and the behavior-specific owner files they affect.

== `core0.option-result-vec`

`core0.option-result-vec` is the Stage 1 standard capability for optional values, recoverable results, and ordered mutable buffers used by diagnostics, lexing, parsing, and other early compiler components. Source code depends on these std APIs, not on descriptor-family names.

The initial API is intentionally small:

```cos
class Option[T] {
  case Some(T)
  case None

  def is_some(&self): Bool
  def is_none(&self): Bool
}

class Result[T, E] {
  case Ok(T)
  case Err(E)

  def is_ok(&self): Bool
  def is_err(&self): Bool
}

class Vec[T] {
  def Vec[T](): Vec[T]
  def push(&mut self, value: T): Unit
  def get(&self, index: usize): T
  def set(&mut self, index: usize, value: T): Unit
  def len(&self): usize
  def size(&self): usize
  def is_empty(&self): Bool
}
```

`Vec[T]` preserves insertion order. `push` appends a value, `len` and `size` return the current element count, `is_empty` is equivalent to `len() == 0`, `get` returns the element at a valid index, and `set` replaces the element at a valid index. Stage 1 callers are responsible for bounds checks before calling `get` or `set`; checked access returning `Option[T]` can be added by a later capability.

`Option[T]` supports construction through `Option[T]::Some(value)` and `Option[T]::None`. `Result[T, E]` supports construction through `Result[T, E]::Ok(value)` and `Result[T, E]::Err(error)`. Matching over these variants is owned by `class.typ`.

The first implementation may lower these APIs through registered descriptor intrinsics such as `descriptor Vec[Token]::push(...)`, but that lowering is a transitional implementation detail. New source-facing collection/result APIs require updates here before they are used by cosmo1 Stage 1 source.

== `core0.text`

`core0.text` is the Stage 1 standard capability for owned text, byte-indexed text views, ASCII-oriented character access, and diagnostic-oriented string construction. Source code depends on this capability and its standard declarations, not on descriptor-family names.

The initial API is intentionally small and byte-oriented:

```cos
class TextView {
  val text: String
  val start: usize
  val end: usize

  def len(&self): usize
  def is_empty(&self): Bool
  def to_string(&self): String
  def byte_at(&self, index: usize): Byte
  def char_at(&self, index: usize): Char
}

class TextBuilder {
  var text: String

  def len(&self): usize
  def is_empty(&self): Bool
  def push(&mut self, value: String): Unit
  def push_view(&mut self, value: TextView): Unit
  def push_line(&mut self, value: String): Unit
  def finish(&self): String
}

def core0_text_len(text: String): usize
def core0_text_is_empty(text: String): Bool
def core0_text_slice(text: String, start: usize, end: usize): String
def core0_text_view(text: String, start: usize, end: usize): TextView
def core0_text_byte_at(text: String, index: usize): Byte
def core0_text_char_at(text: String, index: usize): Char
def core0_text_starts_with(text: String, offset: usize, needle: String): Bool
def core0_text_builder(): TextBuilder
def core0_text_builder_from(text: String): TextBuilder
```

`TextView.start` and `TextView.end` are byte offsets into the owned string used to create the view. `TextView.len()` returns `end - start` when `end > start`, otherwise `0`; callers are responsible for constructing views within source bounds in Stage 1. `byte_at` returns the byte at `start + index`, and `char_at` returns the ASCII-compatible `Char` for that byte. Broader Unicode semantics require a later capability.

`TextBuilder` belongs to `core0.text` because diagnostics need incremental text construction before formatting traits exist. Its first implementation may be source-compiled over immutable `String` concatenation; this does not introduce a `StringBuilder`, `TextBuilder`, `TextView`, or `SourceText` descriptor family.

Cosmo1 Stage 1 source helpers are expected to wrap this capability with source-specific types:

```cos
class SourceText {
  val name: String
  val text: String
}

class SourceMap {
  val source: SourceText
}
```

Those helpers remain ordinary source modules layered on `core0.text`.

== `core0.text-output`

`core0.text-output` is the Stage 1 standard capability for deterministic text sinks used by smoke programs and cosmo1 diagnostics. Source code depends on this capability and its standard declarations, not on descriptor-family names.

The initial API is intentionally small:

```cos
def print(value: String): Unit
def println(value: String): Unit

class TextWriter {
  def write(&self, value: String): Unit
  def write_line(&self, value: String): Unit
}

def core0_stdout(): TextWriter
def core0_stdout_write(value: String): Unit
def core0_stdout_write_line(value: String): Unit
```

The first implementation may be trusted and extern-backed because host output is outside pure cosmo0 source. This does not introduce a `TextWriter`, `Stdout`, `Stderr`, `Output`, or `Console` descriptor family. Diagnostic formatting is owned by cosmo1 driver code, while this capability only owns the sink boundary.

== `core0.path-fs`

`core0.path-fs` is the Stage 1 standard capability for path values and source-file loading. Source code depends on this capability and its standard declarations, not on descriptor-family names.

The initial API is intentionally small:

```cos
class Path {
  val text: String

  def display(&self): String
}

class IoError {
  val path: Path
  val message: String
}

class Fs {
  def read_to_string(path: Path): Result[String, IoError]
}

def core0_path_from_string(text: String): Path
def core0_path_display(path: Path): String
def core0_fs_read_to_string(path: Path): Result[String, IoError]
```

`Path.text` stores the Stage 1 source input name as an owned string. Stage 1 does not define path normalization, canonicalization, directory walking, symlink behavior, current-directory mutation, or platform-specific separator rewriting. `Path.display()` returns the stable source name used in diagnostics and `SourceText.name`.

`Fs.read_to_string` is fallible. Success returns `Result[String, IoError]::Ok(contents)` with the complete file text. Failure returns `Result[String, IoError]::Err(error)` with the attempted path and a stable diagnostic-oriented message. The API must not panic or throw for ordinary missing or unreadable input files.

== `core0.json`

`core0.json` is a later-stage standard capability for transitional JSON loading. It exists so cosmo1 can consume selected parser JSON and package/cache metadata before native source parsing and metadata readers are self-hosted. Source code depends on this standard API, not on descriptor-family names such as `Json` or `JsonValue`.

The initial API is intentionally small:

```cos
class JsonParseError {
  val message: String
}

class JsonValue {
  val handle: String

  def is_null(&self): Bool
  def as_bool(&self): Option[Bool]
  def as_number_text(&self): Option[String]
  def as_string(&self): Option[String]
  def array_len(&self): Option[usize]
  def array_get(&self, index: usize): Option[JsonValue]
  def field(&self, key: String): Option[JsonValue]
}

class Json {
  def parse(&self, text: String): Result[JsonValue, JsonParseError]
}

def core0_json(): Json
def json_parse(text: String): Result[JsonValue, JsonParseError]
def json_is_null(value: &JsonValue): Bool
def json_as_bool(value: &JsonValue): Option[Bool]
def json_as_number_text(value: &JsonValue): Option[String]
def json_as_string(value: &JsonValue): Option[String]
def json_array_len(value: &JsonValue): Option[usize]
def json_array_get(value: &JsonValue, index: usize): Option[JsonValue]
def json_field(value: &JsonValue, key: String): Option[JsonValue]
```

Object and array accessors are fallible and return `Option` so syntax loaders can turn malformed input into diagnostics. Numbers expose their original JSON literal text through `as_number_text`; exact numeric conversion and arbitrary-precision arithmetic belong to later numeric capabilities.

The source-facing parser entry point is `core0_json().parse(text)`. The bodyless `json_parse` declaration is a trusted extern-backed std hook used to implement that entry point; ordinary packages should not treat it as a general host FFI surface.

`core0.json` is not part of the `cosmo1.stage1` profile. Stage 1 validation must continue to pass without this capability, and missing `core0.json` diagnostics are only required for later profiles or packages that explicitly ask for the JSON bridge.

== `core0.char-class`

`core0.char-class` is the Stage 1 standard capability for lexer-oriented ASCII character classification. Source code depends on these ordinary std functions, not on descriptor operations or parser-private character tables.

The initial API is byte-oriented so it composes directly with `core0.text` byte indexing:

```cos
def is_ascii_upper(byte: Byte): Bool
def is_ascii_lower(byte: Byte): Bool
def is_ascii_alpha(byte: Byte): Bool
def is_ascii_digit(byte: Byte): Bool
def is_ascii_alnum(byte: Byte): Bool
def is_ascii_whitespace(byte: Byte): Bool
def is_identifier_start(byte: Byte): Bool
def is_identifier_continue(byte: Byte): Bool
def is_supported_source_byte(byte: Byte): Bool
```

ASCII whitespace is limited to horizontal tab, line feed, carriage return, and space. Identifier starts are `A..Z`, `a..z`, and `_`; identifier continues are identifier starts plus `0..9`. Digits are `0..9`.

Stage 1 treats bytes above `127` as unsupported for lexical classification. A lexer may surface such bytes as invalid tokens or diagnostics, but these helpers must not silently classify non-ASCII bytes as identifier or whitespace characters. Unicode identifier rules require a later capability.

Cosmo1 Stage 1 lexer code is expected to call these helpers directly:

```cos
if (is_identifier_start(source.byte_at(cursor))) {
  scan_identifier()
} else if (is_ascii_digit(source.byte_at(cursor))) {
  scan_number()
}
```

The first implementation may be trusted and extern-backed because source loading needs host filesystem access. That does not introduce `Path`, `IoError`, `Fs`, `Filesystem`, or `File` descriptor families. Generated output writing, recursive package traversal, directory creation, command execution, and richer path manipulation require later capability updates.

Cosmo1 Stage 1 source helpers are expected to wrap this capability with source-specific loading:

```cos
def load_source_text(path: Path): Result[SourceText, IoError] = {
  Fs.read_to_string(path).map(|contents| SourceText(path.display(), contents))
}
```

Those helpers remain ordinary source modules layered on `core0.path-fs` and `core0.text`.

== `core0.arena-id`

`core0.arena-id` is the later-stage standard capability for stable arena storage and typed handles. Source code depends on the `Arena[T]` and `Id[T]` standard APIs, not on descriptor-family names.

The initial API is intentionally small:

```cos
class Id[T]

class Arena[T] {
  def Arena[T](): Arena[T]
  def alloc(&mut self, value: T): Id[T]
  def get(&self, id: Id[T]): &T
  def get_mut(&mut self, id: Id[T]): &mut T
  def len(&self): usize
  def size(&self): usize
}
```

`Arena[T].alloc` appends a value and returns an `Id[T]` that is valid for the same arena item type. `get` returns an immutable reference to the stored value, while `get_mut` requires a mutable arena receiver and returns a mutable reference. `len` and `size` return the number of allocated items. Removal, iteration, numeric ID conversion, cross-arena validation, and serialization are outside the initial capability.

`Id[T]` is phantom typed. Its runtime representation may be an integer-like index, but source typing must preserve the item type so `Id[Expr]` cannot be used where `Id[Ty]` is required.

Cosmo1 syntax AST source is expected to define aliases over these IDs:

```cos
type ExprId = Id[SyntaxExpr]

class SyntaxArenas {
  var exprs: Arena[SyntaxExpr]

  def alloc_expr(&mut self, expr: SyntaxExpr): ExprId = {
    self.exprs.alloc(expr)
  }
}
```

The first implementation may lower these APIs through registered descriptor intrinsics such as `descriptor Arena[Expr]::alloc(...)`, but that lowering is a transitional implementation detail. General map/set storage, garbage collection, serialization, and broader ownership models require later capability updates.

== Descriptor-Backed Transition Policy

Descriptor-backed implementation support is transitional unless a standard API explicitly exposes it. Source code should depend on standard capability identifiers, not on descriptor registry details.

Future descriptor/std proposals must name each changed `docs/cosmo0/` file in their design, tasks, or proposal text. If a change only moves implementation internals and does not alter source-facing behavior, it must say that it is implementation-only and explain why no `docs/cosmo0/` file changes.

== Placeholder API Areas

- Diagnostics, spans, and deterministic output sinks.
- Paths and source-file loading.
- JSON and package metadata bridges.
- Numeric literal preservation and later big-number support.
- Command execution and build integration for later stages.

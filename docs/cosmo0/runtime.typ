= cosmo0 Runtime and Backend

== Status

This file owns runtime and backend requirements behind cosmo0 source behavior. The headings define initial ownership; detailed runtime contracts are placeholders.

== Primitive Descriptors

Primitive descriptors are a closed implementation allowlist. They cover only compiler-essential scalar behavior:

- `Bool`
- `Char`
- `String` scalar backing and currently implemented primitive string helpers
- numeric scalar backing for `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `usize`, `f32`, and `f64`

The descriptor namespace is not the extension point for ordinary runtime APIs. JSON bridges, filesystem APIs, command execution, string builders, deterministic collections, and arbitrary-precision numeric helpers SHALL NOT be added as new runtime descriptor families. Those capabilities belong behind source-facing standard APIs in `std.typ` and may use extern ABI hooks when a backend needs runtime support.

Temporary descriptor-backed lowering remains allowed for the already implemented sealed standard generic families `Vec<T>`, `Option<T>`, `Result<T, E>`, `Arena<T>`, `Id<T>`, `Map<K, V>`, `Set<T>`, `Ptr<T>`, `Box<T>`, `Ref<T>`, and `RefMut<T>`. This compatibility does not make descriptor names public source syntax beyond the type and API rules owned by `type.typ` and `std.typ`.

Any implementation change that adds a descriptor family outside the primitive allowlist or temporary standard generic compatibility set must fail validation until this file, the implementation allowlist, and negative descriptor-boundary tests are updated together.

== Examples

Source-facing call:

```cos
tokens.push(token)
```

Possible lowered runtime operation:

```text
descriptor Vec<Token>::push(%tokens, %token) -> Unit
```

Extern-backed standard API shape:

```cos
import std.io

def smoke(): Unit = {
  println("cosmo1 extern smoke")
}
```

Possible lowered extern obligation:

```text
extern cosmo0.extern.v0 "::cosmo0_runtime::print"
runtime-symbol:cosmo0_runtime::print
include:<cstdio>
```

```text
extern cosmo0.extern.v0 "::cosmo0_runtime::println"
runtime-symbol:cosmo0_runtime::println
include:<cstdio>
```

Filesystem source-loading extern obligation:

```text
extern cosmo0.extern.v0 "::cosmo0_runtime::read_file"
runtime-symbol:cosmo0_runtime::read_file
include:<fstream>
```

JSON parsing bridge extern obligation:

```text
extern cosmo0.extern.v0 "::cosmo0_runtime::json_parse"
runtime-symbol:cosmo0_runtime::json_parse
include:<string>
```

The source-facing call belongs to `std.typ` and expression/type owner files. This file owns the lowered descriptor or extern obligations needed to make that call deterministic and portable.

== Extern ABI Hooks

Extern ABI hooks are backend/runtime obligations used to implement trusted core0/std declarations or compiler-required operations. A hook does not by itself define a source-facing standard API and does not authorize a descriptor family with the same domain name.

The initial ABI name is `cosmo0.extern.v0`. A trusted extern binding records the target runtime symbol as a structured C++ qualified symbol and any backend requirements such as include headers or support libraries. C++ runtime symbols that live in a namespace are emitted as absolute calls, for example `::cosmo0_runtime::println`, while backend requirement keys use the canonical symbol name `cosmo0_runtime::println`. Backends SHALL diagnose an extern binding when the named runtime symbol is not supported by the selected backend.

Extern hooks are not general user-level FFI. cosmo0 source may call the std declarations, such as `print` and `println`, but arbitrary packages may not introduce new bodyless host declarations without accepted metadata.

The trusted `core0.text-output` bindings record the target symbols `::cosmo0_runtime::print` and `::cosmo0_runtime::println` plus the include/runtime support needed by the C++ backend. The source-facing wrappers are owned by `std.typ`; backend failures to provide the runtime symbols are reported as missing extern runtime requirements, not as descriptor failures.

The trusted `core0.path-fs` read binding records the target symbol `::cosmo0_runtime::read_file` and the include/runtime support needed by the C++ backend. The source-facing wrapper returns the standard `Result[String, IoError]` shape owned by `std.typ`; backend failures to provide the runtime symbol are reported as missing extern runtime requirements, not as descriptor failures.

The trusted `core0.json` parse binding records the target symbol `::cosmo0_runtime::json_parse` and the include/runtime support needed by the C++ backend. The source-facing wrapper returns `Result[JsonValue, JsonParseError]` as owned by `std.typ`. The runtime hook is a transitional bridge for selected JSON inputs; it does not authorize `Json`, `JsonValue`, object, array, or parser-AST descriptor families.

== C Extern Function Correspondence

The source form `@extern("c") def name(...)` denotes a trusted direct binding to a C ABI function. The cosmo declaration is the source-facing signature; the extern metadata records the C target symbol and ABI family. A direct C binding has positional argument correspondence: cosmo argument `0` is emitted as C argument `0`, cosmo argument `1` as C argument `1`, and so on. The return type maps to the C result type or `Unit` for `void`.

C header availability is a file-level concern because include order can matter. A source file declares required C headers with semicolon-terminated file decorators. The include kind can be written explicitly or inferred from a `.h` extension:

```cos
@include("stdio.h", kind = "c");
@include("stdlib.h");
```

The backend emits file-level C includes in source order. Direct C extern function decorators SHALL NOT carry `include` arguments.

Example direct binding shape:

```cos
@include("stdio.h");
@extern("c", name = "puts")
def puts(text: CString): i32
```

Corresponding C declaration and emitted call shape:

```c
int puts(const char *text);
puts(text);
```

The declaration is not a C macro binding and is not an arbitrary C expression. It names a callable C function symbol with a stable ABI and direct parameter passing. File-level include requirements make the symbol visible to the generated C++ translation unit; support-library requirements name additional link/runtime obligations when needed.

Variadic C functions require an explicit variadic signature model. A future declaration such as:

```cos
@include("stdio.h");
@extern("c", name = "printf")
def printf(format: StaticCString, ...): i32
```

corresponds to the C prototype:

```c
int printf(const char *format, ...);
```

but support for `...`, format-string validation, signature candidates, and template-like `Any` parameter families is outside the direct-binding subset. Until that later capability is accepted, trusted extern bindings in cosmo0 should use fixed, directly mapped signatures or repository-owned runtime shims such as `::cosmo0_runtime::println`.

== Backend Runtime Requirements

The C++ backend tracks runtime requirements independently from descriptor operations. Requirement records may name descriptor support, runtime symbols, include headers, or support libraries. Descriptor requirements do not create new source APIs, and extern requirements do not create descriptor families.

The backend SHALL emit a diagnostic when a lowered extern binding names a missing runtime symbol. The diagnostic must occur at compile/backend time before a generated C++ artifact is treated as valid.

== Determinism

Placeholder for stable output ordering, unique runtime support emission, and deterministic behavior across repeated package compiles.

== Descriptor Transition Notes

When a standard API is temporarily descriptor-backed, this file should describe only the backend/runtime obligations. Public API shape and source semantics still belong to `std.typ` and the relevant language owner file.

`core0.text` may initially lower through the already permitted primitive `String` backing operations for byte length, emptiness, slicing, ASCII-compatible character access, byte access, and concatenation. Those primitive operations are backend implementation support for the standard API described in `std.typ`; packages must validate source-facing availability through the `core0.text` capability.

Text views, source text wrappers, source maps, and builders are standard/source declarations in Stage 1. They SHALL NOT be registered as runtime descriptor families. In particular, descriptor-backed transition support remains intentionally narrow: the implementation must continue to reject descriptor families such as `Json`, `Filesystem`, `Command`, `StringBuilder`, `TextBuilder`, `TextView`, `SourceText`, and `SourceMap` unless a later OpenSpec change explicitly moves one into the primitive boundary.

`core0.text-output` may lower trusted writes through extern runtime support, but output and writer concepts are not descriptor families. The implementation must continue to reject descriptor families such as `TextWriter`, `Stdout`, `Stderr`, `Output`, and `Console`; source-facing availability is validated through the `core0.text-output` std capability and backend extern requirements.

`core0.path-fs` may lower trusted reads through extern runtime support, but path and filesystem concepts are not descriptor families. The implementation must continue to reject descriptor families such as `Path`, `IoError`, `Fs`, `File`, and `Filesystem`; source-facing availability is validated through the `core0.path-fs` std capability and backend extern requirements.

`core0.json` may lower trusted parsing through extern runtime support, but JSON concepts are not descriptor families. The implementation must continue to reject descriptor families such as `Json`, `JsonValue`, `JsonObject`, and `JsonArray`; source-facing availability is validated through the `core0.json` std capability and backend extern requirements.

`core0.arena-id` may lower through the existing temporary `Arena<T>` and `Id<T>` standard generic descriptor compatibility. The runtime representation may store arena elements in an ordered buffer and represent `Id<T>` as an index-sized value, but the public numeric representation remains opaque. The backend must preserve source-level item typing before lowering so an `Id<Expr>` cannot be used with `Arena<Ty>`. This compatibility does not authorize unrelated ownership or allocation descriptor families such as `Allocator`, `Pool`, `Gc`, `Handle`, or `StablePtr`.

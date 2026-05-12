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
extern cosmo0.extern.v0 "::cosmo0_runtime::println"
runtime-symbol:cosmo0_runtime::println
include:<cstdio>
```

The source-facing call belongs to `std.typ` and expression/type owner files. This file owns the lowered descriptor or extern obligations needed to make that call deterministic and portable.

== Extern ABI Hooks

Extern ABI hooks are backend/runtime obligations used to implement trusted core0/std declarations or compiler-required operations. A hook does not by itself define a source-facing standard API and does not authorize a descriptor family with the same domain name.

The initial ABI name is `cosmo0.extern.v0`. A trusted extern binding records the target runtime symbol as a structured C++ qualified symbol and any backend requirements such as include headers or support libraries. C++ runtime symbols that live in a namespace are emitted as absolute calls, for example `::cosmo0_runtime::println`, while backend requirement keys use the canonical symbol name `cosmo0_runtime::println`. Backends SHALL diagnose an extern binding when the named runtime symbol is not supported by the selected backend.

Extern hooks are not general user-level FFI. cosmo0 source may call the std declaration, such as `println`, but arbitrary packages may not introduce new bodyless host declarations without accepted metadata.

== C Extern Function Correspondence

The source form `@extern("c") def name(...)` denotes a trusted direct binding to a C ABI function. The cosmo declaration is the source-facing signature; the extern metadata records the C target symbol, required headers or libraries, and the ABI family. A direct C binding has positional argument correspondence: cosmo argument `0` is emitted as C argument `0`, cosmo argument `1` as C argument `1`, and so on. The return type maps to the C result type or `Unit` for `void`.

Example direct binding shape:

```cos
@extern("c", symbol = "puts")
@requires(include = "<stdio.h>")
def puts(text: CString): i32
```

Corresponding C declaration and emitted call shape:

```c
int puts(const char *text);
puts(text);
```

The declaration is not a C macro binding and is not an arbitrary C expression. It names a callable C function symbol with a stable ABI and direct parameter passing. Header requirements make the symbol visible to the generated C++ translation unit; support-library requirements name additional link/runtime obligations when needed.

Variadic C functions require an explicit variadic signature model. A future declaration such as:

```cos
@extern("c", symbol = "printf")
@requires(include = "<stdio.h>")
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

Descriptor-backed transition support is intentionally narrow. The implementation must continue to reject descriptor families such as `Json`, `Filesystem`, `Command`, and `StringBuilder` unless a later OpenSpec change explicitly moves them into the primitive boundary.

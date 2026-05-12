= cosmo0 Runtime and Backend

== Status

This file owns runtime and backend requirements behind cosmo0 source behavior. The headings define initial ownership; detailed runtime contracts are placeholders.

== Primitive Descriptors

Primitive descriptors are a closed implementation allowlist. They cover only compiler-essential scalar behavior and required ABI hooks:

- `Runtime` ABI hooks required by the current backend
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
extern "cosmo0" def fs_read_to_string(path: Path): Result<String, IoError>
```

The source-facing call belongs to `std.typ` and expression/type owner files. This file owns the lowered descriptor or extern obligations needed to make that call deterministic and portable.

== Extern ABI Hooks

Extern ABI hooks are backend/runtime obligations used to implement standard APIs or compiler-required operations. A hook does not by itself define a source-facing standard API and does not authorize a descriptor family with the same domain name.

== Backend Runtime Requirements

Placeholder for runtime support emitted by the C++ backend, including helper declarations, deterministic inclusion, standard container lowering, and unsupported-runtime diagnostics.

== Determinism

Placeholder for stable output ordering, unique runtime support emission, and deterministic behavior across repeated package compiles.

== Descriptor Transition Notes

When a standard API is temporarily descriptor-backed, this file should describe only the backend/runtime obligations. Public API shape and source semantics still belong to `std.typ` and the relevant language owner file.

Descriptor-backed transition support is intentionally narrow. The implementation must continue to reject descriptor families such as `Json`, `Filesystem`, `Command`, and `StringBuilder` unless a later OpenSpec change explicitly moves them into the primitive boundary.

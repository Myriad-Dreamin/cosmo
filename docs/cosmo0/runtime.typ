= cosmo0 Runtime and Backend

== Status

This file owns runtime and backend requirements behind cosmo0 source behavior. The headings define initial ownership; detailed runtime contracts are placeholders.

== Primitive Descriptors

Placeholder for descriptor families that implement primitive, standard, or staged runtime behavior. Descriptors are implementation mechanisms unless a source-facing API is specified in `std.typ`.

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

Placeholder for extern function binding, ABI naming, ownership, error reporting, and restrictions needed by standard APIs.

== Backend Runtime Requirements

Placeholder for runtime support emitted by the C++ backend, including helper declarations, deterministic inclusion, standard container lowering, and unsupported-runtime diagnostics.

== Determinism

Placeholder for stable output ordering, unique runtime support emission, and deterministic behavior across repeated package compiles.

== Descriptor Transition Notes

When a standard API is temporarily descriptor-backed, this file should describe only the backend/runtime obligations. Public API shape and source semantics still belong to `std.typ` and the relevant language owner file.

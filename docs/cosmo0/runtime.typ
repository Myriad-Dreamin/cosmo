= cosmo0 Runtime and Backend

== Status

This file owns runtime and backend requirements behind cosmo0 source behavior. The headings define initial ownership; detailed runtime contracts are placeholders.

== Primitive Descriptors

Placeholder for descriptor families that implement primitive, standard, or staged runtime behavior. Descriptors are implementation mechanisms unless a source-facing API is specified in `std.typ`.

== Extern ABI Hooks

Placeholder for extern function binding, ABI naming, ownership, error reporting, and restrictions needed by standard APIs.

== Backend Runtime Requirements

Placeholder for runtime support emitted by the C++ backend, including helper declarations, deterministic inclusion, standard container lowering, and unsupported-runtime diagnostics.

== Determinism

Placeholder for stable output ordering, unique runtime support emission, and deterministic behavior across repeated package compiles.

== Descriptor Transition Notes

When a standard API is temporarily descriptor-backed, this file should describe only the backend/runtime obligations. Public API shape and source semantics still belong to `std.typ` and the relevant language owner file.

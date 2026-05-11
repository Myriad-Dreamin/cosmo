## Context

Some core0 standard APIs cannot be implemented purely in cosmo0 source during early bootstrap. Examples include filesystem reads, process execution, and host output. The API should still belong to std; only the implementation binding should be extern-backed.

## Goals / Non-Goals

**Goals:**

- Define minimal extern binding metadata.
- Track required backend support symbols or libraries.
- Diagnose missing runtime bindings.
- Prove the path with a cosmo1 smoke component that reaches an extern-backed std function.

**Non-Goals:**

- Design the complete C++ FFI.
- Add command execution or filesystem APIs directly.
- Expose extern ABI hooks as general user-level unsafe FFI.

## Decisions

### Extern Binding Behind Std APIs

Extern metadata is attached to trusted core0/std declarations or modules. cosmo1 source calls the std API and does not call arbitrary host symbols directly.

Alternative considered: model each host function as a descriptor operation. That repeats the original descriptor growth problem.

### Track Runtime Requirements Separately

Backend requirement tracking should name support symbols, includes, or runtime libraries, not descriptor operation families.

### Keep The First Smoke Small

The first extern-backed smoke can be a text sink or source-loading shim. It only needs to prove that source-level std calls reach backend runtime requirements.

## Risks / Trade-offs

- Risk: extern hooks become a general FFI too early.
  Mitigation: restrict this proposal to trusted core0/std declarations and document that in `runtime.typ`.

- Risk: backend requirements become target-specific.
  Mitigation: keep metadata abstract enough to map to C++ support code later.

## Migration Plan

1. Update `docs/cosmo0/runtime.typ`, `std.typ`, and `package.typ`.
2. Add extern binding metadata.
3. Add backend runtime requirement tracking.
4. Add diagnostics and smoke tests.

## Open Questions

- Should extern bindings live in package metadata, std declaration metadata, or a dedicated runtime registry?
- Which backend requirement names are stable enough for tests?

## Context

Package and source validation eventually need file access. Stage 1 only needs source file reading, so the first filesystem API should be minimal and std-owned.

## Goals / Non-Goals

**Goals:**

- Define minimal path and filesystem std APIs.
- Bind file reading through extern ABI hooks when host/runtime code is required.
- Add cosmo1 source-loading code that uses the std API.
- Add positive and missing-capability tests.

**Non-Goals:**

- Add command execution.
- Add recursive directory walking or full package loading.
- Define a full path normalization model.

## Decisions

### Read First

`Fs.read_to_string(path)` is the Stage 1 requirement. `Fs.write_string` may be included only if it remains within the same focused boundary; generated output writing can wait.

### Errors Use Standard Result

Filesystem failures should return `Result<String, IoError>` or an equivalent standard fallible type, not throw or panic.

### Path Is A Std Type

`Path` belongs to core0/std. Any host representation is hidden behind extern binding metadata.

## Risks / Trade-offs

- Risk: path semantics vary by host.
  Mitigation: Stage 1 should specify only the path behavior needed by tests and source loading.

- Risk: filesystem APIs grow into package loading.
  Mitigation: keep module graph and package traversal for later proposals.

## Migration Plan

1. Update `docs/cosmo0/std.typ`, `runtime.typ`, and `package.typ`.
2. Add path/filesystem std declarations.
3. Bind read support through extern ABI hooks.
4. Add cosmo1 source loading and tests.

## Open Questions

- Should paths be owned strings, a distinct `Path` class, or both?
- Should write support wait for codegen/output proposals?

## Context

`probe-cosmo0-cte-with-cosmo-jit-sys` validates one tiny request:
`type x = 1 + 1`. That proves the CTE/JIT boundary, but it does not prove the
next scheduling problem: compile-time execution often depends on helper
functions, and those helpers may appear before or after the requesting
declaration in source order. Macro providers will also need recursive helpers
and mutually recursive helper groups.

This change validates the scheduler and execution shape without turning cosmo0
into a full constant-evaluation language.

## Goals / Non-Goals

**Goals:**

- Build a deterministic graph of compile-time callable declarations.
- Resolve call heads early and delay tail/callee resolution until required facts
  are available.
- Compile independent callable groups in dependency order.
- Check each callable header/body at most once per validation plan; repeated
  call sites reuse the same callable facts and artifact.
- Compile recursive callable SCCs as one host artifact when they are otherwise
  supported by the validation profile.
- Execute selected constant function call entries through `cosmo-jit-sys` or a
  request-compatible test double.
- Prove stable diagnostics for unsupported and failing call graph shapes.

**Non-Goals:**

- Define production `const def` syntax or general constant evaluation semantics.
- Interpret function bodies in Scala or JavaScript.
- Add full macro provider execution, reflection metadata, generated
  declarations, or expression macro expansion.
- Accept arbitrary recursive compile-time programs without resource bounds.
- Change ordinary runtime function checking or lowering behavior.

## Decisions

### Keep The Feature Gated

The validation mode should be enabled only by tests, a developer flag, or a
fixture harness. Without the gate, source that looks like compile-time function
evaluation continues to follow existing cosmo0 behavior.

Alternative considered: make constant function calls part of normal cosmo0
immediately. That would expose source-facing semantics before the macro
execution and resource-bound rules are settled.

### Plan From Declaration Headers And Prefix Resolution

The planner first indexes declaration headers and resolves the leading segment
of call expressions. It records delayed call obligations for suffixes or
overload-like cases that need more facts. This index makes source order a
tie-breaker, not a reason to recheck the same helper once for each caller.

For the validation slice, a call dependency edge points from the caller artifact
to the callee artifact after the callee head resolves to a compile-time
callable. Unknown heads and unsupported call shapes produce deterministic
diagnostics. A callable body is checked once, then its checked facts and host
artifact are reused by all entry calls that depend on it.

Alternative considered: scan source items in declaration order and compile each
function when encountered. That misses out-of-order dependencies and does not
prove the scheduling model needed by macro-host execution.

### Compile Callable SCCs As Host Artifacts

Non-recursive callables can be compiled in topological order. A direct or
mutual recursive function group is a strongly connected component. If the SCC
contains only supported compile-time callable declarations, it is compiled as
one host artifact and invoked through a selected entry point.

Alternative considered: reject every recursive call graph. That would avoid
resource-bound complexity, but it would fail to validate the recursive helper
shape that macro providers are expected to need.

### Execute Through The JIT Boundary

The validation result should flow through the same structured
`CosmoJitRequest`/`CosmoJitResult` shape as the existing CTE probe. A pure test
double may be used for deterministic unit tests, but the accepted integration
path routes through `cosmo-jit-sys`.

Alternative considered: add a Scala constant folder for the fixtures. That
would validate neither the macro-host compilation boundary nor C++ execution
through clang-repl.

### Bound Recursive Execution

Recursive fixture execution must have explicit bounds such as timeout,
invocation budget, or JIT adapter limits. Exceeding the bound is a diagnostic,
not a reason to preserve source-order execution.

Alternative considered: rely on host process behavior for non-terminating
recursion. That would make tests flaky and diagnostics unstable.

## Risks / Trade-offs

- Probe syntax can be mistaken for production constant evaluation -> keep the
  mode gated and name diagnostics as validation/probe diagnostics.
- Recursive execution can hang -> enforce adapter-level bounds and failure
  diagnostics.
- JIT availability is platform-sensitive -> split pure graph-planner tests from
  toolchain-backed integration tests.
- Graph planning can overfit to fixtures -> include out-of-source-order,
  dependent, direct recursive, and mutual recursive cases.

## Migration Plan

1. Keep `probe-cosmo0-cte-with-cosmo-jit-sys` as the first JIT boundary smoke.
2. Add the callable graph data model and deterministic planner.
3. Add adapter-compatible CTE requests for callable artifacts and entry calls.
4. Add dependent helper and recursive fixtures.
5. Keep the slice gated until macro-provider execution decides which source
   syntax and profile become production semantics.

## Open Questions

- Whether validation fixtures should use a temporary source marker such as
  `@const` or be injected as harness-level compile-time callable records.
- Which recursion bound should be enforced by the Scala-side adapter versus the
  native `cosmo-jit-sys` layer.

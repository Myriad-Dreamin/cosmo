## Context

cosmoc can compile source files, but it lacks an interactive mode where users or
tests can submit declarations and expressions incrementally. A REPL is not the
same as a final package build: it needs a long-lived session, stable history,
incremental diagnostics, and bounded execution for accepted inputs.

The same engineering shape exists in compile-time evaluation. cosmoc eval owns
provider-entry compilation, PCH/precompiled context caches, artifact invocation,
resource limits, and structured results. REPL mode should use that module
directly instead of adding a separate interpreter.

## Goals / Non-Goals

**Goals:**

- Add a cosmoc REPL mode with an explicit session lifecycle.
- Classify REPL input as declaration, expression/eval request, command, or
  unsupported input.
- Preserve accepted declarations and imports in session state.
- Evaluate accepted expression inputs through cosmoc eval.
- Reuse PCH/precompiled contexts across entries when the context key is stable.
- Provide deterministic diagnostics and display summaries.
- Reject clang-repl / clangInterpreter as the execution substrate.

**Non-Goals:**

- Add a cosmo0 REPL or make cosmo0 depend on cosmoc.
- Use cosmo0 eval from cosmoc REPL.
- Define full production constant evaluation semantics.
- Support arbitrary shell execution or target package process execution.
- Guarantee a polished terminal UI beyond a minimal driver mode.

## Decisions

### Enter REPL Through cosmoc

The user-facing entry point is cosmoc itself, for example a command or option
that starts a REPL session. The REPL owns its source history, accepted
declarations, current package/import context, diagnostics, and eval cache
summary.

Alternative considered: start an external clang-repl process. That bypasses
cosmoc's parser, checker, diagnostics, package model, and eval contract.

### Use cosmoc Eval For Execution

Expression inputs that require execution are lowered into `CosmoEvalRequest`
records handled by cosmoc eval. The request includes the current REPL session
identity, accepted declarations/imports, generated provider-entry source,
target settings, resource limits, precompiled context key, compile options, and
toolchain identity.

Alternative considered: add a REPL-only interpreter. That would duplicate
compile-time evaluation semantics and diverge from provider-entry compilation.

### Keep Session State Explicit

Accepted declarations and imports become session facts. Failed inputs do not
mutate the accepted session state. Commands such as reset or show-cache may
inspect or clear session state without going through expression evaluation.

Alternative considered: append every submitted input to one synthetic source
file and recompile all of it silently. That is easy to prototype but makes
diagnostics, cache reuse, and failed-input rollback unclear.

### Reuse PCH/Precompiled Contexts

The REPL should keep a cache keyed by C++ standard, target triple, toolchain
identity, include paths, headers/imports, compile options, support libraries,
and REPL session profile. When the key is unchanged, later entries can reuse the
cached context.

Alternative considered: cold-compile every REPL entry. That avoids invalidation
questions but makes interactive use too slow for heavy headers.

## Risks / Trade-offs

- REPL state can diverge from batch compilation -> keep accepted session facts
  explicit and add tests comparing selected REPL snippets to equivalent source.
- Cache invalidation can be subtle -> include all compile-affecting settings in
  the key and show cache summaries in diagnostics/debug output.
- Interactive execution can hang -> enforce resource limits and report bounded
  execution diagnostics.
- Users may confuse REPL evaluation with target runtime execution -> document
  that REPL uses cosmoc eval, not the target package binary.

## Migration Plan

1. Add the cosmoc REPL driver entry point and session state model.
2. Add input classification for commands, declarations, expressions, and
   unsupported forms.
3. Route accepted expression inputs through cosmoc eval.
4. Preserve accepted declaration/import facts across entries and roll back
   failed inputs.
5. Add cache summary output and reset/show commands for developer use.
6. Add fixtures for success, diagnostics, cache reuse, reset, and failed-input
   rollback.

## Open Questions

- Which exact command spelling should start REPL mode.
- Which display format should be used for successful values and generated
  summaries.
- Whether multiline input should be handled in the first slice or deferred.

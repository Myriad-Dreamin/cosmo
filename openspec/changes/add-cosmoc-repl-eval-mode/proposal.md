## Why

cosmoc needs an interactive mode for exploratory development, smoke testing,
and compiler debugging. That mode is similar to compile-time evaluation: each
accepted input becomes a bounded evaluation request, the compiler must preserve
session state, and C++ capability should use ordinary Clang compilation with
PCH/precompiled context reuse.

The REPL should not revive the rejected clang-repl / clangInterpreter direction.
It should also not depend on cosmo0 eval. cosmoc is a separate compiler and must
enter REPL mode through its own eval implementation.

## What Changes

- Add a `cosmoc` REPL mode that can be selected by a command or driver option.
- Maintain a REPL session with source history, accepted declarations, imports,
  diagnostics, eval cache state, and resource limits.
- Route executable/evaluable inputs through cosmoc eval using
  `CosmoEvalRequest` / `CosmoEvalResult`.
- Reuse cosmoc eval PCH/precompiled context caches across REPL entries when the
  context key is unchanged.
- Report deterministic diagnostics for unsupported input, failed checking,
  provider-entry compile failure, execution failure, and resource-bound failure.
- Keep cosmo0 eval, macro expansion, production constant-evaluation semantics,
  and target package execution outside this change.

## Capabilities

### New Capabilities

- `cosmoc-repl`: Defines cosmoc's REPL session model, input classification,
  eval integration, PCH/precompiled context reuse, diagnostics, and non-goals.

### Modified Capabilities

- None.

## Impact

- Depends on `add-cosmo-eval-pch-function-compile-poc` for the cosmoc eval
  contract and provider-entry compilation path.
- Adds or extends cosmoc driver entry points for REPL mode.
- Adds REPL fixtures for declarations, expressions, unsupported inputs,
  diagnostics, cache reuse, and session reset.
- Does not make cosmo0 depend on cosmoc and does not make cosmoc depend on
  cosmo0 eval.

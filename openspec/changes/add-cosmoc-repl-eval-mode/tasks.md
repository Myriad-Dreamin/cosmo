## 1. REPL Entry

- [ ] 1.1 Add a cosmoc REPL command or driver option.
- [ ] 1.2 Add a REPL session state model with session id, source history,
  accepted declarations/imports, diagnostics, eval cache summaries, and resource
  limits.
- [ ] 1.3 Add commands for at least reset and session/cache inspection, or
  explicitly defer command support beyond exit/reset.

## 2. Input Classification

- [ ] 2.1 Classify submitted input as REPL command, declaration/import,
  expression/eval request, or unsupported input.
- [ ] 2.2 Preserve accepted declaration/import facts across entries.
- [ ] 2.3 Ensure failed inputs do not mutate accepted session state.
- [ ] 2.4 Emit deterministic diagnostics for unsupported or invalid input.

## 3. Eval Integration

- [ ] 3.1 Lower accepted expression inputs into `CosmoEvalRequest` records for
  cosmoc eval.
- [ ] 3.2 Include REPL session identity, accepted declarations/imports, generated
  provider-entry source, target settings, resource limits, precompiled context
  key, compile options, and toolchain identity in the request.
- [ ] 3.3 Consume `CosmoEvalResult` values and render structured success,
  diagnostic, artifact, and cache summaries.
- [ ] 3.4 Reject clang-repl / clangInterpreter execution paths.

## 4. Cache And Bounds

- [ ] 4.1 Reuse PCH/precompiled context cache entries when the REPL context key
  is unchanged.
- [ ] 4.2 Invalidate or rebuild cache entries when imports, headers, compile
  options, toolchain identity, or session profile changes.
- [ ] 4.3 Enforce execution bounds and report stable bound diagnostics.

## 5. Validation

- [ ] 5.1 Add fixtures for declaration persistence and expression evaluation.
- [ ] 5.2 Add fixtures for failed-input rollback.
- [ ] 5.3 Add fixtures for cache reuse and cache invalidation summaries.
- [ ] 5.4 Add fixtures for unsupported input, compile failure, execution failure,
  and resource-bound failure.
- [ ] 5.5 Run relevant cosmoc tests and `scripts/check-scala-style.sh` if Scala
  sources are edited.

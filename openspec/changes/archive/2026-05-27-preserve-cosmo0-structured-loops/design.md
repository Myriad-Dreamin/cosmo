## Context

The current cosmo0 pipeline preserves source loop nodes through the typed AST,
but `Lowerer.scala` immediately converts `loop`, `while`, and `for` into flat
LIR blocks with `LirBranch` and `LirCondBranch`. `Backend.scala` emits function
bodies by walking those blocks and labels, so generated C++ uses `goto` even for
simple source loops.

The desired shape is a canonical loop with four conceptual slots:

```text
prologue
while condition:
  body
  epilogue
```

That shape must still support cosmo0 `continue`: a `continue` inside the loop
body must run the loop epilogue before the next condition check, while `break`
must leave the loop without running epilogue.

## Goals / Non-Goals

**Goals:**

- Represent source `loop`, `while`, and restricted `for` with one canonical
  loop model after elaboration.
- Preserve loop structure in LIR so the C++ backend can emit `while`, `break`,
  and `continue`.
- Keep `for` descriptor and item-type resolution out of the elaborator.
- Preserve existing flat block/branch support for non-loop control flow and
  fallback cases.
- Make the migration incremental enough to keep current cosmo0 tests useful.

**Non-Goals:**

- Do not fully structure all LIR control flow in this change.
- Do not implement a general CFG-to-structured-code algorithm in the backend.
- Do not change source-language loop syntax.
- Do not change descriptor semantics for `Vec`, `Map`, `Set`, or `Arena`
  iteration beyond how loops carry those operations.

## Decisions

### Use a canonical loop AST instead of three source loop nodes

`UntypedLoop`, `TypedLoop`, and lowered loop handling will converge on a single
canonical shape. The canonical model should support:

- `loop { body }` as condition `Always`.
- `while cond { body }` as condition expression `cond`.
- `for item in iter { body }` as a for-each condition carrying `item`, `iter`,
  and the body.

Alternative considered: leave `TypedLoop`, `TypedWhile`, and `TypedFor` as
separate typed nodes and only unify in `Lowerer.scala`. That reduces duplicate
lowering code but does not give later stages a common loop contract.

### Defer type-driven `for` desugaring until typing/lowering

The elaborator will not synthesize calls such as `iter_has_next` or
`iter_next`. It will preserve the iterator expression and binding name in a
canonical loop condition. The typer will still infer the item type and bind it
in the loop body scope; the lowerer will choose the descriptor intrinsic for the
typed iterable.

Alternative considered: desugar `for` in elaboration into a user-visible
`while` plus synthetic names. That leaks descriptor mechanics into source
syntax, creates hidden-name collision concerns, and asks elaboration to make
type-dependent decisions it cannot make.

### Add structured LIR statements for loop bodies

LIR will gain a structured statement layer for loop-capable function bodies.
At minimum it needs ordinary operation statements, structured loops, returns,
breaks, and continues. A loop condition should be able to carry both setup
operations and a boolean value, because lowering a condition expression may
emit operations before producing the condition local.

Alternative considered: add `LirLoop` as another `LirOp` inside existing
blocks. That makes `break`, `continue`, and `return` awkward because they are
control-flow terminators, not ordinary operations.

### Emit conservative C++ `while (true)` loops first

The backend can always emit the canonical form as:

```cpp
{
  // prologue
  while (true) {
    // condition setup
    if (!condition) {
      break;
    }
    // body
    // epilogue
  }
}
```

When the condition is trivially renderable and has no setup operations, the
backend may emit `while (condition)` as a later refinement.

Alternative considered: always emit `while (condition)`. That fails for lowered
conditions with required setup operations and complicates `for` descriptor
loops.

## Risks / Trade-offs

- [Risk] Two function-body representations may coexist during migration. →
  Mitigation: keep flat LIR support intact and introduce structured bodies only
  where lowering can produce them deterministically.
- [Risk] `continue` can accidentally skip epilogue in generated C++. →
  Mitigation: backend loop emission must maintain an epilogue stack and expand
  structured `continue` into epilogue emission plus native `continue`.
- [Risk] `for` iteration currently relies on runtime iterator-position state
  keyed by collection address. → Mitigation: preserve existing descriptor
  semantics in this change; revisit iterator object design separately.
- [Risk] Tests may assert exact C++ text containing labels. → Mitigation:
  update affected expectations to assert structured `while`, `break`, and
  `continue` output for loop cases while retaining label tests for fallback
  control flow.

## Migration Plan

1. Add canonical loop AST model and adapt elaboration/typing while preserving
   source behavior.
2. Add structured LIR statements and checker coverage without removing flat
   blocks.
3. Move loop lowering to structured LIR for `loop`, `while`, and restricted
   `for`.
4. Update C++ backend emission for structured loops.
5. Keep flat block emission as fallback until more control flow is structured.

Rollback is local: disable structured loop lowering and route loop lowering
back through the existing flat block builder.

## Open Questions

- Should structured LIR be stored beside existing `blocks` in `LirFunction`, or
  should it use a separate function-body sum type?
- Should the first backend implementation optimize pure conditions into
  `while (condition)`, or should all structured loops initially emit
  `while (true)` for uniformity?
- Should `match` be structured in the same pass or deferred until loop
  structure is stable?

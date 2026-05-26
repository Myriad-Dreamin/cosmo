## Why

cosmo0 currently lowers source `loop`, `while`, and `for` constructs into flat
LIR blocks before the C++ backend sees them, forcing generated C++ to express
source-structured loops with labels and `goto`. Preserving loop structure
through elaboration, typing, lowering, and C++ emission will make the backend
output easier to inspect while keeping loop semantics explicit.

## What Changes

- Introduce a canonical source loop representation with `prologue`,
  `condition`, `body`, and `epilogue` parts.
- Desugar source `loop`, `while`, and restricted `for` forms into that single
  canonical loop shape during elaboration.
- Keep type-driven `for` iterator resolution in the typer/lowerer boundary
  instead of resolving descriptor operations in the elaborator.
- Extend cosmo0 LIR with structured loop statements so lowering can preserve
  loop, `break`, and `continue` semantics instead of immediately emitting
  header/body/continue/exit labels.
- Teach the LIR checker to validate structured loops, boolean loop
  conditions, and loop-control placement.
- Update the C++ backend to emit supported structured LIR loops as C++
  `while` loops, using native `break` and `continue` where possible.
- Preserve a flat control-flow fallback for constructs that are not yet
  structured, such as complex lowered match control flow.

## Capabilities

### New Capabilities

- `cosmo0-canonical-loop-elaboration`: Defines canonical source loop
  elaboration for `loop`, `while`, and restricted `for` forms.
- `cosmo0-structured-loop-lir`: Defines structured loop representation,
  lowering, and checking in cosmo0 LIR.

### Modified Capabilities

- `cosmo0-cpp-backend`: Extends backend code generation requirements to emit
  structured LIR loops as C++ `while` loops instead of unconditional label/goto
  control flow.

## Impact

- Affects `packages/cosmo0/src/main/scala/cosmo0/syntax/Untyped.scala`,
  `Typed.scala`, `Elaborator.scala`, and `tyck/mltt/Typer.scala`.
- Affects `packages/cosmo0/src/main/scala/cosmo0/lir/Lir.scala`,
  `Lowerer.scala`, and `TypeChecker.scala`.
- Affects `packages/cosmo0/src/main/scala/cosmo0/backend/cpp/Backend.scala`.
- Requires focused tests for canonical loop elaboration, typed `for` item
  binding, structured LIR checking, and C++ `while` emission.
- Does not add a new runtime dependency or change support-library link
  planning.

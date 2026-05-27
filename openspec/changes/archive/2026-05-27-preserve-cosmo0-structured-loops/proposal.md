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
- Extend structured lowering to preserve supported top-level variant `match`
  expressions so functions that contain both loops and enum dispatch do not
  fall back to label/goto control flow.
- Generate named variant tag enums inside emitted C++ variant structs and use
  those enum values for variant dispatch, avoiding numeric tag magic constants.
- Emit supported variant matches as C++ `switch` statements over named tags,
  while preserving a flat fallback for unsupported pattern forms.
- Use structured lowering by default for supported source control-flow
  constructs so ordinary `if`, short-circuit boolean operators, standard
  `Option`/`Result` matches, user variant matches, and source loops map to
  native C++ control-flow statements without generated `goto`.
- Keep flat block LIR as the explicit low-level control-flow fallback, similar
  to MLIR control-flow blocks and successors, but avoid lowering source-level
  control flow to that fallback when a structured source-to-C++ mapping exists.

## Capabilities

### New Capabilities

- `cosmo0-canonical-loop-elaboration`: Defines canonical source loop
  elaboration for `loop`, `while`, and restricted `for` forms.
- `cosmo0-structured-loop-lir`: Defines structured loop representation,
  lowering, and checking in cosmo0 LIR.
- `cosmo0-structured-variant-match-lir`: Defines structured variant match
  representation, lowering, checking, and C++ emission for top-level enum
  patterns.

### Modified Capabilities

- `cosmo0-cpp-backend`: Extends backend code generation requirements to emit
  structured LIR loops as C++ `while` loops, ordinary branches as C++ `if`
  statements, standard variant matches as C++ `if` chains, and supported user
  variant matches as C++ `switch` statements over named variant tag enums
  instead of unconditional label/goto control flow or numeric tag magic
  constants.

## Impact

- Affects `packages/cosmo0/src/main/scala/cosmo0/syntax/Untyped.scala`,
  `Typed.scala`, `Elaborator.scala`, and `tyck/mltt/Typer.scala`.
- Affects `packages/cosmo0/src/main/scala/cosmo0/lir/Lir.scala`,
  `Lowerer.scala`, and `TypeChecker.scala`.
- Affects `packages/cosmo0/src/main/scala/cosmo0/backend/cpp/Backend.scala`.
- Requires focused tests for canonical loop elaboration, typed `for` item
  binding, structured LIR checking, C++ `while` emission, C++ variant tag enum
  emission, C++ `switch` emission for supported variant matches, and
  goto-free C++ emission for supported source-level control flow.
- Does not add a new runtime dependency or change support-library link
  planning.

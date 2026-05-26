## 1. Canonical Loop Model

- [ ] 1.1 Replace separate untyped loop, while, and for nodes with a canonical untyped loop representation.
- [ ] 1.2 Update elaboration so source `loop`, `while`, and restricted `for` forms produce the canonical loop shape.
- [ ] 1.3 Preserve `for` iterator expression and item binding metadata without synthesizing descriptor operations in elaboration.
- [ ] 1.4 Update untyped debug/rendering or artifact summaries affected by the loop node shape.

## 2. Typed Loop Semantics

- [ ] 2.1 Replace separate typed loop, while, and for nodes with a canonical typed loop representation.
- [ ] 2.2 Update typing for infinite and while loops to produce canonical typed loops with `Unit` result type.
- [ ] 2.3 Update typing for restricted `for` loops to infer the item type and bind the item in the body scope.
- [ ] 2.4 Add or update typer tests for canonical loop typing, invalid while conditions, and invalid iterables.

## 3. Structured LIR

- [ ] 3.1 Add structured LIR statement/body data types for operation statements, loops, returns, breaks, and continues.
- [ ] 3.2 Represent loop conditions with enough data for condition setup operations plus the final Bool value.
- [ ] 3.3 Update lowering to emit structured LIR loops for `loop`, `while`, and restricted `for`.
- [ ] 3.4 Preserve existing flat block lowering for non-loop fallback control flow.
- [ ] 3.5 Update the LIR debug renderer for structured loop bodies.

## 4. LIR Checking

- [ ] 4.1 Extend the LIR type checker to recursively check structured statements.
- [ ] 4.2 Check structured loop condition values as `Bool`.
- [ ] 4.3 Check `break` and `continue` placement with structured loop nesting.
- [ ] 4.4 Add LIR checker tests for valid loops and invalid loop-control placement.

## 5. C++ Backend

- [ ] 5.1 Add structured function-body emission to the C++ backend while preserving flat block emission.
- [ ] 5.2 Emit structured LIR loops as C++ `while` statements.
- [ ] 5.3 Emit structured `break` as native C++ `break`.
- [ ] 5.4 Emit structured `continue` with the active loop epilogue before native C++ `continue`.
- [ ] 5.5 Add backend tests that assert generated C++ uses `while` for representative loop, while, and for inputs.

## 6. Verification

- [ ] 6.1 Run `scripts/check-scala-style.sh` from the repository root and fix any failures.
- [ ] 6.2 Run the cosmo0 Scala test suite covering elaboration, typing, LIR lowering/checking, and C++ emission.
- [ ] 6.3 Compile representative loop programs through the cosmo0 C++ backend and verify the generated C++ is accepted by the configured C++ toolchain.

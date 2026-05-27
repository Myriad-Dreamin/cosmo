## 1. Canonical Loop Model

- [x] 1.1 Replace separate untyped loop, while, and for nodes with a canonical untyped loop representation.
- [x] 1.2 Update elaboration so source `loop`, `while`, and restricted `for` forms produce the canonical loop shape.
- [x] 1.3 Preserve `for` iterator expression and item binding metadata without synthesizing descriptor operations in elaboration.
- [x] 1.4 Update untyped debug/rendering or artifact summaries affected by the loop node shape.

## 2. Typed Loop Semantics

- [x] 2.1 Replace separate typed loop, while, and for nodes with a canonical typed loop representation.
- [x] 2.2 Update typing for infinite and while loops to produce canonical typed loops with `Unit` result type.
- [x] 2.3 Update typing for restricted `for` loops to infer the item type and bind the item in the body scope.
- [x] 2.4 Add or update typer tests for canonical loop typing, invalid while conditions, and invalid iterables.

## 3. Structured LIR

- [x] 3.1 Add structured LIR statement/body data types for operation statements, loops, returns, breaks, and continues.
- [x] 3.2 Represent loop conditions with enough data for condition setup operations plus the final Bool value.
- [x] 3.3 Update lowering to emit structured LIR loops for `loop`, `while`, and restricted `for`.
- [x] 3.4 Preserve existing flat block lowering for non-loop fallback control flow.
- [x] 3.5 Update the LIR debug renderer for structured loop bodies.

## 4. LIR Checking

- [x] 4.1 Extend the LIR type checker to recursively check structured statements.
- [x] 4.2 Check structured loop condition values as `Bool`.
- [x] 4.3 Check `break` and `continue` placement with structured loop nesting.
- [x] 4.4 Add LIR checker tests for valid loops and invalid loop-control placement.

## 5. C++ Backend

- [x] 5.1 Add structured function-body emission to the C++ backend while preserving flat block emission.
- [x] 5.2 Emit structured LIR loops as C++ `while` statements.
- [x] 5.3 Emit structured `break` as native C++ `break`.
- [x] 5.4 Emit structured `continue` with the active loop epilogue before native C++ `continue`.
- [x] 5.5 Add backend tests that assert generated C++ uses `while` for representative loop, while, and for inputs.
- [x] 5.6 Emit named variant `Tag` enums inside generated C++ variant structs and use the named enum values for tag checks.
- [x] 5.7 Preserve supported top-level variant `match` expressions in structured LIR instead of forcing whole functions back to flat blocks.
- [x] 5.8 Emit supported structured variant matches as C++ `switch` statements over named variant tags.
- [x] 5.9 Add backend/lowering tests that assert representative enum matches use named `Tag` cases and no numeric tag magic constants.
- [x] 5.10 Use structured lowering by default for supported source control flow so ordinary `if`, short-circuit boolean operators, loops, and matches emit native C++ control flow instead of `goto`.
- [x] 5.11 Emit standard `Option`/`Result` structured matches as C++ `if`/`else` chains and avoid `switch` when a match arm contains source `break`.

## 6. Verification

- [x] 6.1 Run `scripts/check-scala-style.sh` from the repository root and fix any failures.
- [x] 6.2 Run the cosmo0 Scala test suite covering elaboration, typing, LIR lowering/checking, and C++ emission.
- [x] 6.3 Compile representative loop programs through the cosmo0 C++ backend and verify the generated C++ is accepted by the configured C++ toolchain.
- [x] 6.4 Re-run Scala style checks after structured variant match changes.
- [x] 6.5 Re-run targeted cosmo0 tests covering LIR lowering, backend emission, and cosmoc bootstrap C++ output.
- [x] 6.6 Verify the regenerated cosmoc C++ render output contains zero `goto` tokens.

## 1. Checker Structure

- [ ] 1.1 Add a LIR checker entry point that accepts a LIR module and returns structured diagnostics.
- [ ] 1.2 Build checker environments for functions, locals, labels, declarations, call signatures, variants, and descriptor intrinsics.
- [ ] 1.3 Reuse cosmo0 diagnostic and source span types where LIR nodes retain source origin.

## 2. Structural Validation

- [ ] 2.1 Validate function signatures, parameter locals, local declarations, and local use-before-definition rules.
- [ ] 2.2 Validate every block has exactly one valid terminator.
- [ ] 2.3 Validate branch targets exist and branch conditions have boolean type.
- [ ] 2.4 Validate all reachable return terminators match the function return type.

## 3. Type Validation

- [ ] 3.1 Validate assignment, field get/set, direct call, descriptor intrinsic, variant construction, tag read, and payload read operation types.
- [ ] 3.2 Validate reference and mutability invariants that must hold after lowering.
- [ ] 3.3 Validate variant payload shape and match-lowering support operations.
- [ ] 3.4 Validate deterministic descriptor operation signatures used by later backend emission.

## 4. Tests

- [ ] 4.1 Add hand-written valid LIR fixtures that pass the checker.
- [ ] 4.2 Add negative LIR fixtures for missing terminators, invalid branch targets, type mismatches, invalid calls, invalid variants, and invalid mutability.
- [ ] 4.3 Add tests proving checker diagnostics are deterministic and identify the failing LIR construct.

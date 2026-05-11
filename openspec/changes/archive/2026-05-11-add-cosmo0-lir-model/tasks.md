## 1. LIR Core Data Model

- [x] 1.1 Define LIR modules, declarations, functions, parameters, locals, labels, blocks, operations, values, and terminators.
- [x] 1.2 Define LIR type references and callable signatures using cosmo0 source type information where needed.
- [x] 1.3 Define explicit local identifiers and declaration identifiers that are stable across rendering and tests.

## 2. LIR Operations

- [x] 2.1 Add LIR operations for local allocation, assignment, field get/set, direct calls, method-like lowered calls, descriptor intrinsics, variant construction, tag reads, and payload reads.
- [x] 2.2 Add LIR terminators for return, unconditional branch, conditional branch, and unreachable/error exits.
- [x] 2.3 Represent structured source control flow only through explicit blocks and terminators.

## 3. Inspection Utilities

- [x] 3.1 Add a deterministic debug renderer for LIR modules and functions.
- [x] 3.2 Add helper constructors for concise hand-written LIR tests.
- [x] 3.3 Add tests proving rendering is stable for equivalent LIR inputs.

## 4. Boundary Checks

- [x] 4.1 Ensure LIR nodes do not depend on shared parser syntax nodes.
- [x] 4.2 Ensure LIR nodes do not depend on full-language `cosmo.ir` type-level, reflection, staging, or quotation constructs.

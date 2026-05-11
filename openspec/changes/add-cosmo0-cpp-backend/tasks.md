## 1. Backend Structure

- [x] 1.1 Add a C++ backend entry point that accepts verified LIR modules.
- [x] 1.2 Add backend context for deterministic namespaces, symbol names, local names, includes, and support-code requirements.
- [x] 1.3 Reject unverified or structurally invalid LIR at the backend boundary.

## 2. C++ Type And Declaration Emission

- [x] 2.1 Emit C++ type names for scalar types, references, user declarations, variants, and registered standard generic descriptors.
- [x] 2.2 Emit class and field declarations from LIR layout information.
- [x] 2.3 Emit variant representation and helper operations required by lowered variant and match LIR.
- [x] 2.4 Emit function and method signatures from LIR callable declarations.

## 3. Function Body Emission

- [x] 3.1 Emit C++ locals, assignments, field get/set, direct calls, descriptor calls, returns, and unreachable exits.
- [x] 3.2 Emit C++ control flow for LIR branches, conditional branches, loops, and lowered match dispatch.
- [x] 3.3 Emit descriptor runtime includes or support code for lowered standard generic and runtime operations.

## 4. Determinism And Tests

- [x] 4.1 Add golden or snapshot tests for representative verified LIR inputs.
- [x] 4.2 Add tests proving repeated emission of the same verified LIR is byte-for-byte stable.
- [x] 4.3 Add tests proving backend output does not depend on full Cosmo compiler codegen.

## 5. Initial Compile Acceptance Targets

- [x] 5.1 Convert or replace `packages/cosmoc/src/parser.cos` so it is a parser library source with no `main` function and only backend-supported cosmo0 constructs.
- [x] 5.2 Add compile validation proving `packages/cosmoc/src/parser.cos` emits C++ accepted by the configured C++ toolchain as a library-style target.
- [x] 5.3 Add compile validation proving `samples/HelloWorld/main.cos` emits C++ accepted by the configured C++ toolchain as an executable target.
- [x] 5.4 Keep `parser_test.cos` executable fixture-running behavior out of this backend proposal and covered by the dedicated parser test program proposal.

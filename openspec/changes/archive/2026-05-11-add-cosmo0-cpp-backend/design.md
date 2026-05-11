## Context

The cosmo0 backend sits after checked LIR. It must not recover source-level
semantics from parser, typer, or lowerer state. Its first useful milestone is a
small set of compile targets that prove the backend can emit C++ for both a
library module and an executable program.

## Backend Boundary

`CppBackend` will accept `LirModule` values and re-run `LirTypeChecker` at the
entry point. A successful backend run therefore means the input is structurally
valid at the LIR boundary and the emitted C++ is derived only from LIR.

Invalid LIR returns `Result.failure(Phase.Compile, diagnostics)` with the checker
diagnostics or backend-specific unsupported-operation diagnostics. The backend
does not call the existing full Cosmo C++ code generator.

## Output Strategy

The first implementation emits one deterministic C++ translation unit per LIR
module. Package-level multi-file output belongs to the package pipeline change,
but this backend must expose enough metadata for that pipeline to gather runtime
descriptor requirements later.

Generated output uses stable ordering:

- declarations by LIR declaration id
- fields and variants by source name
- functions by declaration id
- locals by local id
- blocks by label id

Names are produced through a backend context that sanitizes modules,
declarations, locals, labels, and descriptor support symbols. Collisions are
resolved deterministically from the original ids.

## Initial Compile Targets

`packages/cosmoc/src/parser.cos` is the first library-shaped target. For this
change it must contain parser library code only; it must not define `main`.
Compile validation for this target proves the backend can emit linkable C++ for
a module that is consumed by another program.

`samples/HelloWorld/main.cos` is the first executable-shaped target. Compile
validation for this target proves the backend can emit a C++ `main` program for
ordinary source that uses string output, integer locals, numeric operations, and
basic expression statements.

`parser_test.cos` is intentionally outside this backend change. It is tracked by
the separate `add-cosmoc-parser-test-program` proposal because it owns a `main`
function and fixture execution behavior rather than the backend's core emission
contract.

## Descriptor Runtime

The backend will initially support the descriptor operations already lowered
into LIR for scalar arithmetic and comparison, booleans, strings,
`StringBuilder`, `Vec`, `Option`, and `Result`. Additional containers such as
`Map`, `Set`, and `Arena` can be added as needed by the parser library target.

Unsupported descriptor operations fail with backend diagnostics rather than
being silently emitted as invalid C++.

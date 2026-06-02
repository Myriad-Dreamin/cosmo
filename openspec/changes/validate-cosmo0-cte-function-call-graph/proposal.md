## Why

The current compile-time evaluation probe proves one literal arithmetic request,
but macro-host execution will need helper functions, dependent compile-time
calls, and recursive call graphs. cosmo0 needs a focused validation slice that
proves compile-time call scheduling can use a dependency graph instead of a
source-order loop.

## What Changes

- Add a gated compile-time evaluation validation mode for constant function call
  fixtures.
- Build a compile-time callable graph from parsed declaration headers and
  prefix-first call resolution.
- Validate function calls whose callees depend on other constant functions,
  including source-order-independent helper lookup.
- Validate direct and mutual recursive constant function calls by compiling a
  callable strongly connected component as one host artifact.
- Route execution through the existing `cosmo-jit-sys` request/result boundary
  or an adapter-compatible test double.
- Report deterministic diagnostics for unresolved call heads, unsupported call
  shapes, unavailable JIT execution, and recursion that does not finish within
  the validation bounds.
- Keep general constant evaluation, arbitrary type-level arithmetic, macro
  providers, generated declarations, and production `const` semantics out of
  scope for this change.

## Capabilities

### New Capabilities

- `cosmo0-cte-function-call-graph`: Defines the gated compile-time function
  call graph validation slice, including dependent constant calls, recursive
  callable SCCs, JIT execution, and stable diagnostics.

### Modified Capabilities

- None.

## Impact

- Depends on `probe-cosmo0-cte-with-cosmo-jit-sys` for the structured CTE/JIT
  request boundary.
- Adds a small planner for compile-time callable headers and call obligations.
- Adds focused fixtures for dependent helper calls, source-order-independent
  helper lookup, direct recursion, mutual recursion, cycle/timeout diagnostics,
  and deterministic result ordering.
- Does not change normal cosmo0 compile-time behavior unless the validation
  mode is explicitly enabled.

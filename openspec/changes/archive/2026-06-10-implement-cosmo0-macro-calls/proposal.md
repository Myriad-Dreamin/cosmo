## Why

The macro-system design defines `Expr[Untyped]` and normalized macro payloads,
but cosmo0 still needs an implementation slice that proves expression macro
calls can be selected, invoked, and rechecked. A focused macro-call proposal
keeps this separate from derive reflection and full self-hosted macro packages.

## What Changes

- Add a gated implementation slice for expression macro calls.
- Resolve macro call targets through ordinary prefix-first name/member/tag
  resolution rather than textual name matching.
- Normalize accepted call-site syntax into one provider-facing payload:
  `Expr.Args`, `Expr.Block`, or `Expr.Template`.
- Invoke compiler-hosted smoke providers through the macro function
  input/output boundary.
- Validate generated `Expr[Untyped]` output and re-enter ordinary name
  resolution and type checking.
- Add deterministic diagnostics for disabled macro calls, unresolved providers,
  invalid provider signatures, unsupported payload shapes, invalid output, and
  expansion cycles.
- Keep derive macros, self-hosted macro packages, provider package dependency
  graphs, and arbitrary typed quotation out of scope for this slice.

## Capabilities

### New Capabilities

- `cosmo0-expression-macro-calls`: Defines expression macro call selection,
  single-payload normalization, provider invocation, generated expression
  validation, and diagnostics.

### Modified Capabilities

- None.

## Impact

- Depends on `introduce-cosmo0-macro-system` for the macro value boundary and
  compile-time evaluation contract.
- Uses the prefix-first resolution model documented in
  `docs/cosmo/name-resolution.typ`.
- Adds compiler-hosted smoke providers and fixtures before self-hosted provider
  package execution exists.
- Touches parser/elaborator preservation, resolver classification, macro
  expansion, type checking reintegration, and diagnostics in implementation.

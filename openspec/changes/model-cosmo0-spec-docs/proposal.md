## Why

cosmo0 staged runtime work needs a stable written model before more descriptor or standard library behavior is added. Without a `docs/cosmo0/` spec set, implementation fixes can silently redefine the subset and cosmo1 source can start relying on undocumented compiler behavior.

## What Changes

- Add the `docs/cosmo0/` specification skeleton.
- Define ownership for `spec.typ`, `type.typ`, `class.typ`, `expr.typ`, `control-flow.typ`, `std.typ`, `runtime.typ`, `package.typ`, and `testing.typ`.
- Document the bug/spec sync rule used by future cosmo0 changes.
- Add a Stage 1 capability profile placeholder that later changes can fill.
- Add lightweight validation that the spec skeleton exists and future staged runtime proposals name their spec updates.

## Capabilities

### New Capabilities

- `cosmo0-spec-docs`: Establishes the cosmo0 model docs and spec synchronization policy.

### Modified Capabilities

- `cosmo0-staged-runtime-capabilities`: Uses `docs/cosmo0/` as the authority for descriptor/std capability behavior.

## Impact

- Adds documentation structure only; no compiler behavior changes.
- Gives GitHub reviewers a stable place to check intended cosmo0 semantics.
- Makes future descriptor/std PRs responsible for updating the relevant spec files.

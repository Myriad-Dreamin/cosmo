## Why

cosmo0 must eventually check and compile compiler code spread across multiple files. A package-level pipeline makes module loading, import ordering, dependency diagnostics, and stable multi-module output explicit.

## What Changes

- Add package-level cosmo0 check and compile entry points.
- Load package metadata and source modules for cosmo0-targeted packages.
- Resolve module imports and public declarations across files.
- Diagnose unsupported dependency cycles.
- Apply deterministic module ordering for checking and output.
- Connect package-level compilation to the existing cosmo0 phases.

## Capabilities

### New Capabilities

- `cosmo0-package-pipeline`: Defines package-level loading, dependency ordering, checking, and compilation for cosmo0-targeted packages.

### Modified Capabilities

None.

## Impact

- Adds package orchestration code to `packages/cosmo0`.
- Introduces cosmo0 package-level test fixtures.
- Leaves current package loading for the full compiler unchanged.

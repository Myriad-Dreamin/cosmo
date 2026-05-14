## Why

The parser self-compile path needs a way to select source files and resolve imports before semantic analysis can run. This change defines cosmo1 package metadata loading and module graph behavior at the narrow boundary needed for `parser.cos`.

## What Changes

- Add package metadata loading for cosmo1 validation slices.
- Define source selection and module ordering for parser self-compile inputs.
- Add import edge and module graph diagnostics for missing modules and cycles.

## Capabilities

### New Capabilities

- `cosmo1-package-module-graph`: Defines package metadata, source selection, imports, and module graph behavior for parser self-compile.

### Modified Capabilities

None.

## Impact

- Future implementation will extend `packages/cosmoc/src/package` modules.
- Enables single-file parser self-compile first while preserving a path to package validation.

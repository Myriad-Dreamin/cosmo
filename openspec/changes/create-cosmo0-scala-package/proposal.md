## Why

cosmo0 should be developed as a separate Scala compiler path rather than as conditional behavior inside the existing full Cosmo compiler. A dedicated package boundary keeps bootstrap-oriented code isolated and makes it easier to preserve current full-language behavior.

## What Changes

- Create a new `packages/cosmo0` Scala package.
- Add the initial cosmo0 public API surface for parsing, checking, and compiling through the future pipeline.
- Add shared result and diagnostic types needed by later phases.
- Wire the package into the build without changing the behavior of `packages/cosmo`.
- Add minimal tests proving the cosmo0 package can be loaded and invoked independently.

## Capabilities

### New Capabilities

- `cosmo0-scala-package`: Defines the independent Scala package, API boundary, diagnostics, and build integration for the cosmo0 compiler path.

### Modified Capabilities

None.

## Impact

- Adds a new Scala package under `packages/cosmo0`.
- Updates build configuration to include the new package.
- Leaves the existing `packages/cosmo` compiler behavior unchanged.

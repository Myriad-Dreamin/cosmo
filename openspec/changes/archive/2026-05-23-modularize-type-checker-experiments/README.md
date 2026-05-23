# modularize-type-checker-experiments

Define modular type checker architecture so cosmo0 and cosmoc can host multiple checking experiments with explicit feature profiles.

## Implementation Notes

- `cosmoc.basic-expr` is the default profile for `packages/cosmoc/src/types/check.cos` and produces `typed-expression-map` artifacts.
- `cosmo0.subset` is the default cosmo0 checker profile and produces `typed-module` artifacts.
- `mltt.core` is metadata-only in this change. It is selectable by tests but returns unsupported-feature diagnostics until a concrete MLTT checker implementation lands.
- Profile selection remains a development/test harness feature. `Cosmo0.checkWithProfile` and package `checkerProfile` metadata route only the default `cosmo0.subset` checker to implementation; non-default profiles are isolated as unsupported results.
- Package-level checker profile selection is intentionally separate from `stageProfile` capability validation.

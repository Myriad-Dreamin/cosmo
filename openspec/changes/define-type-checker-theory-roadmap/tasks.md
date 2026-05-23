## 1. Roadmap Review

- [ ] 1.1 Review the theory profile registry and remove profiles that are too speculative.
- [ ] 1.2 Decide initial conformance levels for existing `cosmoc.basic-expr` and future `mltt.core`.
- [ ] 1.3 Decide whether conformance metadata lives in OpenSpec, fixture manifests, or test reports.
- [ ] 1.4 Decide the first performance envelope fields required for compiler integration.

## 2. MLTT Alignment

- [ ] 2.1 Update the MLTT proposal to target `mltt.core` plus a first WHNF conversion strategy.
- [ ] 2.2 Move stronger normalization strategies out of the first MLTT implementation plan.
- [ ] 2.3 Remove advanced normalization claims from the MLTT documentation until research is complete.
- [ ] 2.4 Add explicit unsupported-feature diagnostics for effectful conversion and unsupported normalization profiles.

## 3. Common Goal Suite

- [ ] 3.1 Define the common checking goal names and result schema in a follow-up implementation change.
- [ ] 3.2 Define fixture manifest fields for profile id, required features, goals, expected diagnostics, and artifact summaries.
- [ ] 3.3 Add common harness fixtures that every checker must run.
- [ ] 3.4 Add deterministic artifact-summary expectations for repeated checker runs.
- [ ] 3.5 Define how cosmo0-side reference implementations report the same profile goals as `packages/cosmoc`.

## 4. Normalization Profiles

- [ ] 4.1 Define `mltt.whnf-conversion` goal tests.
- [ ] 4.2 Reserve space for future normalization profiles without naming one before research.
- [ ] 4.3 Define the minimum information a future normalization proposal must provide.
- [ ] 4.4 Define deterministic overflow and fuel diagnostics for normalization.

## 5. Integration Gates

- [ ] 5.1 Define L0 through L4 profile conformance levels in checker metadata.
- [ ] 5.2 Define which level is required for package checking.
- [ ] 5.3 Define which level is required for LSP use.
- [ ] 5.4 Add reporting that lists each checker profile, supported features, conformance level, and missing test groups.
- [ ] 5.5 Require cosmo0-side experiments to remain profile-gated until they reach the integration level required for package or LSP use.

## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/spec.typ` with staged capability validation.
- [ ] 1.2 Update `docs/cosmo0/std.typ` with capability identifiers.
- [ ] 1.3 Update `docs/cosmo0/package.typ` with stage profile selection.
- [ ] 1.4 Update `docs/cosmo0/testing.typ` with capability validation test policy.

## 2. Registry

- [ ] 2.1 Add primitive descriptor requirement data to stage profiles.
- [ ] 2.2 Add core0/std capability requirement data to stage profiles.
- [ ] 2.3 Add missing-capability diagnostics.
- [ ] 2.4 Ensure later-stage capabilities do not block Stage 1.

## 3. Cosmo1 Component

- [ ] 3.1 Add Stage 1 package metadata/profile scaffolding for `packages/cosmo1`.
- [ ] 3.2 Connect the Stage 1 profile to package validation.

## 4. Tests

- [ ] 4.1 Add positive Stage 1 capability validation tests.
- [ ] 4.2 Add negative tests for missing `core0.text`, `core0.path-fs`, and primitive intrinsics.
- [ ] 4.3 Add tests proving JSON, command, arena, map/set, and big-number capabilities are not required for Stage 1.

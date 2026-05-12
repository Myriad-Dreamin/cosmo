## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/spec.typ` with staged capability validation.
- [x] 1.2 Update `docs/cosmo0/std.typ` with capability identifiers.
- [x] 1.3 Update `docs/cosmo0/package.typ` with stage profile selection.
- [x] 1.4 Update `docs/cosmo0/testing.typ` with capability validation test policy.

## 2. Registry

- [x] 2.1 Add primitive descriptor requirement data to stage profiles.
- [x] 2.2 Add core0/std capability requirement data to stage profiles.
- [x] 2.3 Add missing-capability diagnostics.
- [x] 2.4 Ensure later-stage capabilities do not block Stage 1.

## 3. Cosmo1 Component

- [x] 3.1 Add Stage 1 package metadata/profile scaffolding for `packages/cosmoc`.
- [x] 3.2 Connect the Stage 1 profile to package validation.

## 4. Tests

- [x] 4.1 Add positive Stage 1 capability validation tests.
- [x] 4.2 Add negative tests for missing `core0.text`, `core0.path-fs`, and primitive intrinsics.
- [x] 4.3 Add tests proving JSON, command, arena, map/set, and big-number capabilities are not required for Stage 1.

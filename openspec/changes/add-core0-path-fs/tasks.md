## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/std.typ` with `Path`, `IoError`, and `Fs.read_to_string`.
- [ ] 1.2 Update `docs/cosmo0/runtime.typ` with filesystem extern binding requirements.
- [ ] 1.3 Update `docs/cosmo0/package.typ` with source-loading expectations.

## 2. Standard API

- [ ] 2.1 Add `Path` std declarations or source module.
- [ ] 2.2 Add `IoError` std declarations.
- [ ] 2.3 Add `Fs.read_to_string` returning a standard fallible result.
- [ ] 2.4 Optionally add `Fs.write_string` only if it stays in the same boundary.

## 3. Cosmo1 Components

- [ ] 3.1 Add or extend `source/source.cos` to load source text by path.
- [ ] 3.2 Add or extend `driver/config.cos` input path handling.

## 4. Tests

- [ ] 4.1 Add fixture file read tests through std FS.
- [ ] 4.2 Add missing `core0.path-fs` capability diagnostics.
- [ ] 4.3 Add cosmo1 source loading validation tests.
- [ ] 4.4 Add tests proving filesystem APIs are std/extern-backed, not descriptors.

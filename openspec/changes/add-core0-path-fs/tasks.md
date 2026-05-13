## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/std.typ` with `Path`, `IoError`, and `Fs.read_to_string`.
- [x] 1.2 Update `docs/cosmo0/runtime.typ` with filesystem extern binding requirements.
- [x] 1.3 Update `docs/cosmo0/package.typ` with source-loading expectations.

## 2. Standard API

- [x] 2.1 Add `Path` std declarations or source module.
- [x] 2.2 Add `IoError` std declarations.
- [x] 2.3 Add `Fs.read_to_string` returning a standard fallible result.
- [x] 2.4 Leave `Fs.write_string` out of this focused source-loading boundary.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend `source/source.cos` to load source text by path.
- [x] 3.2 Add or extend `driver/config.cos` input path handling.

## 4. Tests

- [x] 4.1 Add fixture file read tests through std FS.
- [x] 4.2 Add missing `core0.path-fs` capability diagnostics.
- [x] 4.3 Add cosmo1 source loading validation tests.
- [x] 4.4 Add tests proving filesystem APIs are std/extern-backed, not descriptors.

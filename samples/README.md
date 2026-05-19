# cosmo0 Samples

This directory is the positive cosmo0 sample corpus. Keep examples here inside
the current cosmo0 subset: concrete classes, explicit primitive and standard
types, sealed standard generics, package imports, ordinary control flow, and
runtime calls that the cosmo0 backend already knows.

Current samples are discovered from comment directives rather than a manifest:

- `/// expect: compile` marks a single `.cos` source file that must compile.
- `/// expect: compile-package` marks a file inside a package whose nearest
  ancestor `cosmo.json` directory must compile as a package.

Historical full-Cosmo samples live under `samples/legacy/`. They are useful as
language-design references, but new compiler acceptance examples should be added
to the focused cosmo0 directories at this level.

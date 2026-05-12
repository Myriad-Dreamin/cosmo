= cosmo0 Packages and Stage Validation

== Status

This file owns package metadata, source loading, import graphs, module ordering, and staged validation behavior for cosmo0.

== Package Metadata

Placeholder for package manifest fields, target selection, source-root configuration, and diagnostics for invalid package metadata.

== Examples

Minimal package metadata shape:

```json
{
  "name": "cosmo1-stage1-smoke",
  "version": "0.0.0",
  "target": "cosmo0",
  "root": "src"
}
```

Source-loading shape for a first-stage package:

```text
src/source.cos
src/token.cos
src/lexer.cos
```

Import shape that should resolve within the package graph:

```cos
import source.SourceText
import token.Token
```

The exact metadata schema remains a placeholder, but examples show the intended target boundary: a cosmo0 package declares its bootstrap target, discovers source files under a stable root, and resolves imports deterministically.

== Imports and Source Loading

Placeholder for source discovery, import resolution, package-relative module paths, filesystem API dependencies, and source text ownership.

== Module Ordering

Placeholder for deterministic module ordering, import-cycle diagnostics, and package-level check or compile sequencing.

== Stage Validation

Placeholder for validating cosmo1 compiler stages through cosmo0-compiled code. Stage validation should name the required standard and runtime capabilities rather than assuming full Cosmo availability.

== Stage 1 Capability Profile

The Stage 1 package placeholder covers source-file loading, module discovery for the first validation package, stable diagnostic inputs, and deterministic smoke output. Exact acceptance tests are tracked by the OpenSpec change `validate-cosmo1-stage1-through-cosmo0`.

This file will be updated when Stage 1 validation needs package metadata, multi-file loading, or source-root behavior beyond the current placeholder.

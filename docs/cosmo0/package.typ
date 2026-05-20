= cosmo0 Packages and Stage Validation

== Status

This file owns package metadata, source loading, import graphs, module ordering, and staged validation behavior for cosmo0.

== Package Metadata

Placeholder for package manifest fields, target selection, source-root configuration, and diagnostics for invalid package metadata.

== Examples

Minimal package metadata shape:

```json
{
  "name": "@cosmo/compiler",
  "version": "0.0.0",
  "target": "cosmo0",
  "root": "src",
  "stageProfile": "cosmo1.stage1",
  "sources": [
    "parser.cos",
    "parser_test.cos"
  ]
}
```

Source-loading shape for a first-stage package:

```text
src/core0/text.cos
src/core0/text_output.cos
src/core0/path_fs.cos
src/core0/char_class.cos
src/source/span.cos
src/source/source.cos
src/source/source_map.cos
src/driver/diagnostic.cos
src/lex/token.cos
src/lex/lexer.cos
```

Import shape that should resolve within the package graph:

```cos
import source.SourceText
import token.Token
```

C++ namespace imports are source-level foreign aliases, not Cosmo package dependencies:

```cos
import std as cstd from "c++/vector"
```

The exact metadata schema remains small: a cosmo0 package declares its bootstrap target, discovers source files under a stable root, optionally selects a stage profile, may restrict validation to an ordered `sources` list, and resolves imports deterministically.

== Imports and Source Loading

Package source loading uses package-root-relative source names and deterministic module paths. When `sources` is present, each entry is interpreted relative to the package source root and loaded in the listed order. When `sources` is absent, source discovery must produce a stable order before import resolution.

Stage 1 source loading depends on `core0.path-fs` for file reads and `core0.text` for owned source text helpers. A loaded file is represented as ordinary cosmo1 source data, such as `SourceText`, whose `name` is the stable path display string and whose `text` is the complete file contents.

Missing, unreadable, or invalid source paths are ordinary package-check diagnostics. They must not be reported as descriptor lookup failures, and they must not require package metadata to name arbitrary host filesystem symbols.

C++ namespace imports do not add package graph edges. The package graph only follows ordinary Cosmo module imports. A C++ namespace import contributes backend include and foreign-binding inputs owned by `runtime.typ` and `name-resolution.typ`; it must not produce `cosmo0.package.missing-import` for a `c++/<header>` path.

== Runtime Binding Availability

Package check may accept trusted bodyless std declarations only when the compiler can attach accepted extern ABI metadata. Package compile SHALL fail if the selected backend cannot satisfy the extern runtime symbol, include, or support-library requirements recorded during lowering.

Packages do not declare arbitrary host symbols in manifest metadata. Runtime binding availability is derived from trusted std/core0 declarations and the backend requirement records owned by `runtime.typ`.

== Module Ordering

Placeholder for deterministic module ordering, import-cycle diagnostics, and package-level check or compile sequencing.

== Stage Validation

Stage validation uses the optional `stageProfile` metadata field. When present, the value names a registered capability profile such as `cosmo1.stage1`.

Package check validates that the selected profile is known and that required primitive descriptors, core0 standard capabilities, and backend extern/runtime requirements are available. Missing requirements are reported as package-check diagnostics before backend emission.

When `sources` is present, package validation loads only those source-root-relative files. This lets `packages/cosmoc` keep exploratory files near the compiler while validating a narrower Stage 1 slice.

Extern-backed smoke validation should import and call the std API it is proving. A smoke package that only needs output may depend on `std.io.println`; it should not pull in filesystem or command APIs unless the stage being validated explicitly requires those capabilities.

== Stage 1 Capability Profile

The Stage 1 package profile is `cosmo1.stage1`. It covers source-file loading, module discovery for the first validation package, stable diagnostic inputs, text operations, spans, source maps, token definitions, lexing, ASCII character classification, and deterministic smoke output.

The first scaffolded consumer lives at `packages/cosmoc` and selects `stageProfile: "cosmo1.stage1"`. Exact acceptance tests for the complete Stage 1 package are tracked by the OpenSpec change `validate-cosmo1-stage1-capability-profile`.

The profile requires `core0.path-fs` for source-file loading. Later package graph traversal, recursive directory walking, generated artifact writing, and command execution remain outside this first source-loading boundary unless a later profile or capability explicitly adds them.

== Later Build, Link, And Run Profiles

Later profiles that orchestrate external tools may require `core0.command`. That capability covers structured command construction, explicit argument and environment data, optional working directory selection, and captured exit status, stdout, and stderr. A package or profile that needs compiler, linker, test-runner, or other external process execution must request `core0.command` explicitly.

The `cosmo1.stage1` profile must continue to validate when `core0.command` is unavailable. Missing command support is only diagnosed for a later profile or package check that explicitly requires it, using `cosmo0.stage.missing-capability` for `core0.command`.

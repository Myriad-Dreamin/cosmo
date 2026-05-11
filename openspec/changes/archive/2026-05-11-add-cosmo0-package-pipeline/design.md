## Context

cosmo0 previously exposed file-level parse, check, lower, and compile entry
points. Later bootstrap stages need the same phases over a package directory so
compiler code can be split across several `.cos` files with explicit imports
and deterministic generated output.

The existing full Cosmo package loader remains in `packages/cosmo`; this change
adds an isolated cosmo0 package path under `packages/cosmo0`.

## Goals / Non-Goals

**Goals:**

- Load cosmo0 package metadata from `cosmo.json`.
- Discover cosmo0 sources under the package source root.
- Resolve intra-package imports, order modules deterministically, and reject
  missing imports and dependency cycles.
- Run the existing cosmo0 parse, elaboration, source type, LIR lower, LIR check,
  and C++ backend phases over package inputs.
- Preserve stable package output and leave the full compiler package loader
  unchanged.

**Non-Goals:**

- Cross-package dependency loading.
- Incremental compilation or separate C++ translation units per module.
- Full visibility enforcement beyond the public declarations represented by the
  current cosmo0 syntax model.

## Decisions

Package metadata is loaded by a new cosmo0-only pipeline. The supported metadata
fields are `name`, `version`, optional `root`, and optional `target`. When
`target` is present it must be `cosmo0`; this lets packages opt into the
bootstrap path without changing full Cosmo metadata behavior.

Package modules are discovered as `.cos` files under `root` or `src` by default.
The module path is the source-root-relative path without `.cos`, split by
directories. Imports resolve to these module paths.

The package graph is checked before source typing. Missing modules, missing
imported declarations, duplicate module paths, duplicate public declarations,
and dependency cycles are reported as package diagnostics with stable ordering.

A checked package is represented as a deterministic package-ordered combined
module for the existing cosmo0 type, LIR, and backend phases. This keeps the
first package implementation small while still making module ordering explicit.
Separate output units can be added later by changing the package output model
without changing file-level phases.

## Risks / Trade-offs

- Current parser nodes do not preserve `pub` or `private`, so public declaration
  checks treat top-level non-import declarations as public. Later visibility work
  can refine this without changing the module graph.
- Combining modules into one backend output avoids cross-translation-unit symbol
  work now, but it means package compilation does not yet emit one C++ file per
  source module.
- Source type names are still unqualified by module. Duplicate package-level
  declarations are rejected to avoid ambiguous merged-module lowering.

## Migration Plan

The change is additive. Existing file-level cosmo0 APIs and the full Cosmo
package loader remain available. Package callers can use the new `loadPackage`,
`checkPackage`, and `compilePackage` facade methods. Rollback removes the new
package pipeline files, fixtures, and tests without touching the full compiler
package path.

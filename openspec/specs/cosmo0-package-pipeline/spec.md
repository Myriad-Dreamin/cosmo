# cosmo0-package-pipeline Specification

## Purpose
TBD - created by archiving change add-cosmo0-package-pipeline. Update Purpose after archive.
## Requirements
### Requirement: Package Metadata Loading
cosmo0 SHALL load package metadata for package-level check and compile
operations from a package root containing `cosmo.json`.

#### Scenario: Valid metadata is accepted
- **WHEN** a cosmo0 package contains `cosmo.json` with `name`, `version`, and an optional `target` of `cosmo0`
- **THEN** cosmo0 loads the package metadata and uses `src` as the default source root when `root` is absent

#### Scenario: Missing metadata is diagnosed
- **WHEN** a package root does not contain `cosmo.json`
- **THEN** cosmo0 rejects the package with a missing metadata diagnostic

#### Scenario: Unsupported target is diagnosed
- **WHEN** package metadata declares a `target` other than `cosmo0`
- **THEN** cosmo0 rejects the package as an unsupported cosmo0 package configuration

### Requirement: Package Source Discovery
cosmo0 SHALL discover package modules from `.cos` files under the configured
source root.

#### Scenario: Source files are discovered deterministically
- **WHEN** a package contains one or more `.cos` files under the source root
- **THEN** cosmo0 loads them as source-root-relative module paths in stable order

#### Scenario: Missing sources are diagnosed
- **WHEN** a package has no `.cos` files under the configured source root
- **THEN** cosmo0 rejects the package with a missing sources diagnostic

### Requirement: Module Import Graph
cosmo0 SHALL resolve package imports to discovered modules and build a
deterministic dependency graph before package checking.

#### Scenario: Acyclic imports are ordered
- **WHEN** a package module imports another package module and the graph is acyclic
- **THEN** cosmo0 checks dependency modules before importing modules

#### Scenario: Missing imports are diagnosed
- **WHEN** a module imports a package module that does not exist
- **THEN** cosmo0 rejects the package with a missing import diagnostic

#### Scenario: Import cycles are diagnosed
- **WHEN** package modules form a dependency cycle
- **THEN** cosmo0 rejects the package with a diagnostic naming the cycle

### Requirement: Package Check Pipeline
cosmo0 SHALL expose a package check entry point that runs parse, elaboration,
source typing, LIR lowering, and LIR checking over all modules in deterministic
dependency order.

#### Scenario: Single-module package checks
- **WHEN** a valid cosmo0 package contains one source module
- **THEN** cosmo0 package check succeeds and returns the checked package model

#### Scenario: Multi-module package checks
- **WHEN** a valid cosmo0 package contains multiple acyclic source modules
- **THEN** cosmo0 package check succeeds and preserves stable module ordering

### Requirement: Package Compile Pipeline
cosmo0 SHALL expose a package compile entry point that emits deterministic C++
output for checked packages.

#### Scenario: Package compile emits stable output
- **WHEN** the same valid cosmo0 package is compiled repeatedly without source changes
- **THEN** generated module ordering, namespaces, symbols, runtime requirements, and C++ output are stable

#### Scenario: Runtime requirements are unique per output
- **WHEN** package compilation emits descriptor-backed runtime requirements
- **THEN** each runtime requirement is reported once for the generated output unit

### Requirement: Full Compiler Package Loading Is Unchanged
Adding cosmo0 package-level loading SHALL NOT change existing full Cosmo package
loading behavior.

#### Scenario: Full compiler package tests still pass
- **WHEN** the existing full Cosmo package tests run after adding cosmo0 package loading
- **THEN** full compiler package loading remains governed by the existing package loader


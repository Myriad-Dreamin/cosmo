## 1. Package Inputs

- [x] 1.1 Define the cosmo0 package metadata fields required for check and compile operations.
- [x] 1.2 Add package source discovery for cosmo0-targeted source files.
- [x] 1.3 Add package-level diagnostics for missing metadata, missing source files, and unsupported package configuration.

## 2. Module Graph

- [x] 2.1 Extract module imports from elaborated cosmo0 source.
- [x] 2.2 Resolve imports to package modules and public declarations.
- [x] 2.3 Build a deterministic module dependency graph.
- [x] 2.4 Diagnose dependency cycles that cosmo0 cannot compile.

## 3. Package Check And Compile

- [x] 3.1 Add package-level check that runs parse, elaboration, source typing, LIR lowering, and LIR checking over all modules in dependency order.
- [x] 3.2 Add package-level compile that emits backend outputs for checked modules.
- [x] 3.3 Ensure package compilation includes descriptor runtime requirements once per output unit according to backend strategy.
- [x] 3.4 Preserve stable module, namespace, symbol, and output ordering.

## 4. Tests

- [x] 4.1 Add package fixtures for single-module, multi-module acyclic, missing import, and import-cycle cases.
- [x] 4.2 Add tests for package check diagnostics.
- [x] 4.3 Add tests for deterministic package compile output across repeated runs.
- [x] 4.4 Confirm package-level cosmo0 behavior does not change full Cosmo package loading behavior.

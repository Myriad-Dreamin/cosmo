## 1. Import Model And Parsing

- [x] 1.1 Extend the parser/elaborator import model to represent Cosmo module imports separately from C++ namespace imports.
- [x] 1.2 Accept `import <cpp-namespace> as <alias> from "c++/<header>"` for C++ namespace imports.
- [x] 1.3 Reject `import "c++/<header>"` with an unsupported C++ header-only import diagnostic.
- [x] 1.4 Add import parsing tests for valid namespace imports, merge-compatible repeated imports, incompatible aliases, and rejected header-only imports.

## 2. Name Resolution

- [x] 2.1 Add a foreign namespace alias binding kind with canonical C++ namespace path, local alias, contributing headers, and source span data.
- [x] 2.2 Merge repeated C++ namespace imports only when the local alias and canonical C++ namespace target match.
- [x] 2.3 Report conflicts when a C++ namespace alias collides with a Cosmo declaration/import or maps the same alias to a different C++ namespace.
- [x] 2.4 Resolve qualified paths by resolving the leftmost segment first and dispatching subsequent segments by binding kind.
- [x] 2.5 Ensure C++ aliases do not make the original C++ namespace name or any nested C++ symbol implicitly visible.

## 3. Package Graph And Backend Inputs

- [x] 3.1 Exclude C++ namespace imports from Cosmo package dependency edges.
- [x] 3.2 Preserve contributing C++ header sources as backend include requirements or foreign binding inputs.
- [x] 3.3 Route C++ qualified suffix validation through `cosmo-clang-sys` using the alias canonical namespace and bounded header set.

## 4. cosmo-clang-sys

- [x] 4.1 Add the `cosmo-clang-sys` native source layout and C ABI header for `cosmo_clang_sys_*` exported functions.
- [x] 4.2 Add CMake configuration for `cosmo-clang-sys`, including Clang/LLVM dependency discovery and a clear missing-dependency diagnostic.
- [x] 4.3 Produce `libcosmoClang.a` on Linux when `cosmo-clang-sys` is enabled.
- [x] 4.4 Implement bounded header parsing and symbol query operations for namespace, suffix, header-set requests.
- [x] 4.5 Link `cosmoc` against `libcosmoClang.a` when C++ namespace import support is enabled.

## 5. Documentation

- [x] 5.1 Add a name-resolution document that formalizes resolver inputs, outputs, phase order, unqualified lookup, qualified lookup, foreign namespace aliases, and diagnostics.
- [x] 5.2 Update syntax/import documentation to state that `import "c++/<header>"` is unsupported and that C++ imports require an explicit namespace alias.
- [x] 5.3 Document C++ namespace merge imports with the `std as cstd` from `"c++/vector"` and `"c++/string"` examples.
- [x] 5.4 Document the `cosmo-clang-sys` CMake build, Linux `libcosmoClang.a` artifact, and `cosmoc` linkage contract.

## 6. Fixtures And Tests

- [x] 6.1 Add `fixtures/name-resolution` positive fixtures for lexical lookup, qualified lookup, and compatible C++ namespace alias merging.
- [x] 6.2 Add `fixtures/name-resolution` negative fixtures for unresolved names, duplicate bindings, incompatible C++ namespace aliases, Cosmo/C++ alias conflicts, and unsupported header-only C++ imports.
- [x] 6.3 Add `cosmo-clang-sys` CMake/build tests or smoke checks for Linux `libcosmoClang.a` production.
- [x] 6.4 Wire resolver tests to run the `fixtures/name-resolution` corpus and check stable diagnostic codes/spans.
- [x] 6.5 Run the relevant parser, package graph, name-resolution, CMake, `cosmo-clang-sys`, and fixture tests.

Validation note: `COSMO_ENABLE_CLANG_SYS=ON` configuration was exercised on the current host and reached the intended missing Clang CMake package diagnostic. Producing `libcosmoClang.a` requires a host LLVM/Clang install exposed through `COSMO_LLVM_PATH`.

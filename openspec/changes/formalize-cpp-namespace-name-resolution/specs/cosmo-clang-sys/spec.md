## ADDED Requirements

### Requirement: CMake-Built Clang Support Library

The repository SHALL provide a native `cosmo-clang-sys` support component built by CMake for Clang-backed C++ header parsing and symbol validation.

#### Scenario: Linux static library artifact is produced

- **WHEN** the CMake build configures `cosmo-clang-sys` on Linux with the required Clang/LLVM development components available
- **THEN** the build produces a static library artifact named `libcosmoClang.a`
- **AND** the artifact is available to link into the `packages/cosmoc` executable package build

#### Scenario: Missing Clang dependency is diagnosed

- **WHEN** the CMake build enables `cosmo-clang-sys` but cannot find the required Clang/LLVM development components
- **THEN** CMake reports a clear configuration diagnostic naming the missing Clang dependency
- **AND** the build does not silently fall back to ad hoc C++ header parsing

### Requirement: Narrow C ABI Boundary

`cosmo-clang-sys` SHALL expose a stable C ABI for `cosmoc` and SHALL NOT expose Clang or LLVM C++ types across the library boundary.

#### Scenario: ABI uses stable value and handle shapes

- **WHEN** `cosmoc` calls into `cosmo-clang-sys`
- **THEN** exported symbols use a `cosmo_clang_sys_` prefix
- **AND** ABI values use fixed-width scalar fields, byte spans or C strings, opaque handles, explicit release functions, and structured status or error records
- **AND** Clang and LLVM implementation types remain private to `cosmo-clang-sys`

#### Scenario: Parser resources are released through the library

- **WHEN** `cosmoc` receives an opaque handle from `cosmo-clang-sys`
- **THEN** `cosmo-clang-sys` provides the matching release function
- **AND** ownership of Clang parser/index resources does not escape into `cosmoc`

### Requirement: Header Parsing And Symbol Query API

`cosmo-clang-sys` SHALL provide the C++ header parsing and bounded symbol query operations required by C++ namespace imports.

#### Scenario: Symbol query is bounded by explicit headers

- **WHEN** `cosmoc` asks `cosmo-clang-sys` to validate namespace `::std`, suffix `vector`, and header set `"c++/vector"`
- **THEN** `cosmo-clang-sys` parses only the requested translation-unit/header configuration
- **AND** returns whether the symbol exists in the bounded import context
- **AND** does not search unrelated headers that were not part of the request

#### Scenario: Symbol metadata is deterministic

- **WHEN** `cosmo-clang-sys` validates a C++ symbol successfully
- **THEN** it returns deterministic metadata sufficient for later type checking or lowering
- **AND** the metadata includes the canonical C++ qualified name, symbol kind, source header identity, and diagnostics or notes needed to explain failures

### Requirement: cosmoc Links Against cosmo-clang-sys

`cosmoc` SHALL consume C++ header parsing through the `cosmo-clang-sys` library artifact when C++ namespace import support is enabled.

#### Scenario: cosmoc links Linux static artifact

- **WHEN** `cosmoc` is built on Linux with C++ namespace import support enabled
- **THEN** the `packages/cosmoc` package build links its generated executable target against `libcosmoClang.a`
- **AND** C++ namespace import validation calls go through the `cosmo-clang-sys` C ABI

#### Scenario: cosmoc does not parse C++ headers directly

- **WHEN** the C++ namespace import implementation is inspected
- **THEN** `cosmoc` delegates Clang translation-unit setup, header parsing, and C++ symbol queries to `cosmo-clang-sys`
- **AND** compiler name resolution remains responsible only for Cosmo binding rules and the bounded query requests sent to `cosmo-clang-sys`

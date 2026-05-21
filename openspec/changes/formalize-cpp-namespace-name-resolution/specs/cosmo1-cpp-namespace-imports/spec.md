## ADDED Requirements

### Requirement: C++ Namespace Merge Imports

cosmo1 SHALL support C++ namespace imports that bind a C++ namespace path to an explicit local alias from a named C++ header source.

#### Scenario: C++ namespace alias resolves the leftmost symbol

- **WHEN** a source file contains `import std as cstd from "c++/vector"` followed by a reference to `cstd::vector`
- **THEN** name resolution resolves `cstd` as an explicit C++ foreign namespace alias for `::std`
- **AND** records `"c++/vector"` as a header source associated with the alias
- **AND** leaves `vector` as the remaining C++ symbol path for `cosmo-clang-sys` to validate against the imported header set

#### Scenario: Compatible C++ namespace imports merge

- **WHEN** a source file contains `import std as cstd from "c++/vector"` and `import std as cstd from "c++/string"`
- **THEN** name resolution creates one local alias binding named `cstd`
- **AND** the binding targets the canonical C++ namespace `::std`
- **AND** the binding records both `"c++/vector"` and `"c++/string"` as contributing header sources

#### Scenario: Incompatible C++ namespace imports conflict

- **WHEN** a source file contains two C++ namespace imports with the same local alias but different canonical C++ namespace targets
- **THEN** name resolution reports a duplicate or conflicting foreign namespace alias diagnostic before type checking

### Requirement: Header-Only C++ Imports Are Unsupported

cosmo1 SHALL reject C++ header-only imports because they do not introduce an explicit leftmost symbol for qualified name resolution.

#### Scenario: Bare C++ header import is rejected

- **WHEN** a source file contains `import "c++/vector"`
- **THEN** cosmo1 reports an unsupported C++ header-only import diagnostic
- **AND** the import does not add a Cosmo module dependency
- **AND** the import does not make `std`, `vector`, or any other C++ symbol visible

### Requirement: C++ Symbols Do Not Implicitly Cover Cosmo Symbols

cosmo1 SHALL keep C++ foreign namespace aliases explicit and SHALL NOT let C++ symbols conflict with, shadow, or implicitly cover Cosmo declarations or imports.

#### Scenario: C++ alias conflicts with Cosmo declaration

- **WHEN** a scope declares a Cosmo symbol named `cstd` and also imports `std as cstd from "c++/vector"` in the same scope
- **THEN** name resolution reports a duplicate or conflicting binding diagnostic
- **AND** neither binding silently shadows the other

#### Scenario: C++ namespace is not implicitly visible

- **WHEN** a source file contains `import std as cstd from "c++/vector"` and then references `std::vector`
- **THEN** name resolution reports `std` as unresolved unless another declaration explicitly binds `std`
- **AND** the C++ namespace alias `cstd` does not implicitly cover the Cosmo name `std`

### Requirement: C++ Header Sources Are Bounded By Explicit Imports

cosmo1 SHALL resolve C++ qualified paths only through the header sources attached to the explicit foreign namespace alias used as the leftmost path segment.

#### Scenario: Resolver does not search unrelated headers

- **WHEN** `cstd` is introduced only by `import std as cstd from "c++/vector"`
- **AND** source references `cstd::stringstream`
- **THEN** name resolution resolves only the `cstd` alias root
- **AND** later C++ symbol validation through `cosmo-clang-sys` is limited to the header set attached to `cstd`
- **AND** cosmo1 does not search all C++ standard library headers to satisfy `stringstream`

### Requirement: C++ Header Symbol Validation Uses Clang

cosmo1 SHALL validate C++ symbol suffixes for C++ namespace imports through the Clang-backed `cosmo-clang-sys` integration rather than by ad hoc header parsing.

#### Scenario: Imported C++ symbol is validated by cosmo-clang-sys

- **WHEN** name resolution has resolved `cstd` from `import std as cstd from "c++/vector"`
- **AND** the source references `cstd::vector`
- **THEN** cosmo1 passes the canonical namespace `::std`, suffix `vector`, and the alias header set to `cosmo-clang-sys`
- **AND** `cosmo-clang-sys` validates the symbol using Clang header parsing
- **AND** the resolver result records the validated foreign symbol metadata for later type checking or lowering

#### Scenario: Missing C++ symbol is diagnosed from bounded header set

- **WHEN** `cosmo-clang-sys` cannot find the requested suffix inside the explicit header set attached to a C++ namespace alias
- **THEN** cosmo1 reports a stable unresolved C++ symbol diagnostic
- **AND** the diagnostic does not depend on unrelated C++ headers that were not imported through that alias

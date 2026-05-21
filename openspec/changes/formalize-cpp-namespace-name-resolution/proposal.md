## Why

Cosmo needs a deterministic way to reference C++ declarations without scanning every imported header or letting foreign symbols leak into ordinary Cosmo lookup. The current name-resolution specification is too small to describe C++ namespace aliases, conflict handling, or the exact lookup phases that later type checking and lowering depend on.

## What Changes

- Add explicit C++ namespace merge imports of the form `import std as cstd from "c++/vector"`.
- Reject header-only C++ imports such as `import "c++/vector"`; C++ imports must introduce an explicit namespace alias.
- Allow repeated C++ namespace imports to merge when they bind the same local alias to the same C++ namespace, for example importing `std as cstd` from both `"c++/vector"` and `"c++/string"`.
- Require C++ namespace aliases to stay in a separate foreign namespace binding space that cannot conflict with, implicitly cover, or shadow Cosmo symbols.
- Add `cosmo-clang-sys`, a CMake-built Clang-backed native support library that provides `libcosmoClang.a` on Linux for the `packages/cosmoc` executable package's header and symbol validation.
- Formalize cosmo1 name resolution as a documented phase with deterministic collection, conflict validation, import classification, lexical lookup, qualified lookup, and diagnostic behavior.
- Require name-resolution fixtures under `fixtures/name-resolution` that cover ordinary Cosmo lookup and C++ namespace import behavior.

## Capabilities

### New Capabilities

- `cosmo1-cpp-namespace-imports`: Defines C++ namespace alias imports, merge semantics, header-only import rejection, and foreign namespace isolation.
- `cosmo-clang-sys`: Defines the CMake-built Clang integration library, Linux artifact contract, C ABI boundary, and `packages/cosmoc` consumption requirements for C++ header parsing.

### Modified Capabilities

- `cosmo1-name-resolution`: Formalizes the resolver algorithm and extends resolution behavior to account for explicit foreign namespace aliases without changing ordinary Cosmo lexical lookup.

## Impact

- Parser/elaborator import representation will need to distinguish Cosmo module imports from C++ namespace imports.
- The module graph must ignore C++ header imports as Cosmo package dependencies while retaining their backend header requirements.
- Name resolution must collect foreign namespace aliases, merge compatible aliases, reject conflicts, and resolve qualified paths through explicit leftmost aliases.
- The CMake build must produce a Linux static `libcosmoClang.a` artifact, and the `packages/cosmoc` package build must link it when Clang-backed C++ header parsing is enabled.
- Documentation must include the formal name-resolution algorithm and its inputs, outputs, phases, and diagnostics.
- Test fixtures must be added under `fixtures/name-resolution` for positive and negative resolver cases.

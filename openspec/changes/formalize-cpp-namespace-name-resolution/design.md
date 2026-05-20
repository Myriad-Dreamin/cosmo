## Context

Cosmo currently treats imports primarily as Cosmo module dependencies, while the C++ backend already has include requirements and C++ qualified symbol machinery. A bare C++ header import such as `import "c++/vector"` does not give the resolver a bounded root symbol for paths like `std::vector`; resolving that form would either require global header search or implicit foreign symbol injection. Both behaviors make name resolution non-deterministic and create conflicts with Cosmo module names such as `std`.

The resolver should instead require an explicit local alias for a C++ namespace root. After:

```cos
import std as cstd from "c++/vector"
import std as cstd from "c++/string"
```

the leftmost symbol in `cstd::vector` is a resolved foreign namespace alias. The resolver can then continue lookup inside the merged C++ namespace import set without scanning unrelated headers or treating `std` as a Cosmo binding.

C++ header parsing and symbol validation should be delegated to `cosmo-clang-sys`, a native CMake-built library backed by Clang. On Linux, this component provides `libcosmoClang.a` for `cosmoc`. The resolver remains responsible for deterministic Cosmo binding rules; Clang is responsible for validating and describing C++ symbols within the explicit header set attached to a foreign namespace alias.

## Goals / Non-Goals

**Goals:**

- Reject `import "c++/..."` as an unsupported header-only import form.
- Support explicit C++ namespace aliases with merge semantics across multiple headers.
- Keep C++ symbols out of ordinary Cosmo lexical and module lookup unless introduced by an explicit alias.
- Provide a CMake-built `cosmo-clang-sys` native library for C++ header parsing and symbol validation.
- Define a deterministic name-resolution phase, including inputs, outputs, phase order, conflict checks, qualified lookup, and diagnostics.
- Add documentation and fixtures that make the algorithm testable.

**Non-Goals:**

- Parse every declaration from a C++ header.
- Support C++ overload resolution, ADL, concepts, macros, operators, or template substitution in this change.
- Treat `std` or any other C++ namespace as implicitly visible after a C++ import.
- Make `std::vector` resolve unless `std` itself was explicitly imported as a local alias.
- Define ABI-safe adapters between C++ standard library types and Cosmo standard types.
- Make `cosmoc` depend directly on Clang C++ APIs outside the `cosmo-clang-sys` boundary.

## Decisions

### C++ Imports Bind Namespaces, Not Headers

C++ imports use:

```cos
import <cpp-namespace> as <local-alias> from "c++/<header>"
```

The import target names a C++ namespace path, and the local alias introduces the resolver root. A header-only form is rejected because it records an include but does not define the leftmost symbol used by qualified lookup.

Alternative considered: allow `import "c++/vector"` and make `std::vector` work. That requires implicit global bindings or header-wide search. Both make conflicts with Cosmo `std` ambiguous and make lookup depend on the contents of headers that were not explicitly named in source.

### Compatible Imports Merge By Alias And C++ Namespace

Two C++ namespace imports merge only when they use the same local alias and the same canonical C++ namespace:

```cos
import std as cstd from "c++/vector"
import std as cstd from "c++/string"
```

The merged binding records the union of headers. If the same alias points to a different C++ namespace, or if the alias conflicts with a Cosmo declaration/import in the same scope, resolution reports a duplicate or conflicting binding diagnostic.

Alternative considered: let later imports shadow earlier imports. Shadowing would make available C++ symbols order-dependent and would violate the rule that foreign symbols cannot implicitly cover Cosmo symbols.

### Foreign Namespace Bindings Are Separate But Explicit

The resolver records foreign namespace aliases in a separate binding kind. Ordinary unqualified lookup can find the alias name itself, but only as an explicit foreign namespace root; nested C++ symbols remain accessible only through `::` qualified lookup from that root.

This means `cstd::vector` can resolve after `import std as cstd from "c++/vector"`, while `std::vector` remains unresolved unless `std` is explicitly imported as the local alias. A Cosmo symbol named `cstd` in the same scope conflicts with the import instead of being silently shadowed.

### Name Resolution Has A Fixed Phase Order

The formal resolver document and implementation should use these phases:

1. Collect module declarations, import declarations, class members, function parameters, block locals, and pattern-bound names into candidate binding records.
2. Classify imports as Cosmo module/member imports or C++ namespace imports.
3. Validate duplicate and conflict rules within each scope before resolving references.
4. Build lexical scopes and explicit import bindings.
5. Resolve unqualified names by walking from innermost lexical scope outward.
6. Resolve qualified paths by resolving the leftmost segment first, then walking member, module, type, or foreign namespace children according to the resolved binding kind.
7. Emit stable definition ids or foreign symbol ids for resolved references and diagnostics for unresolved, ambiguous, duplicate, or unsupported forms.

Keeping collection and validation before expression lookup prevents later references from changing meaning based on traversal order.

### Clang Integration Lives Behind cosmo-clang-sys

`cosmo-clang-sys` is a native support component built by CMake. It owns the Clang dependency, translation-unit setup, include path handling, header parsing, and C++ symbol query API. `cosmoc` links against the library artifact instead of embedding Clang-specific code in the compiler front end.

On Linux, the release artifact is a static library named `libcosmoClang.a`. The library exposes a narrow C ABI with stable `cosmo_clang_sys_*` symbols. ABI values use fixed-width integers, C strings or byte spans, caller-owned input buffers, opaque handles for parser/index state, explicit release functions, and structured status/error results. Clang or LLVM C++ types must not cross the ABI boundary.

Alternative considered: call libclang or Clang C++ APIs directly from `cosmoc`. Direct calls make the compiler binary own Clang version differences and resource lifetime details. A dedicated library gives the build one integration point and keeps Clang-specific diagnostics and data conversion isolated.

## Risks / Trade-offs

- C++ namespace imports do not prove that a referenced child symbol exists until the C++ header index or binding generator is available. Mitigation: name resolution resolves the explicit alias root and records the remaining C++ path for later C++ symbol validation.
- Clang packaging varies across Linux distributions. Mitigation: CMake must detect the required Clang/LLVM components, expose a clear configuration diagnostic when unavailable, and keep `cosmo-clang-sys` optional until C++ namespace imports are enabled by the build profile.
- Users may expect `import std as cstd from "c++/vector"` to import every C++ standard library symbol. Mitigation: the spec says the import provides a namespace root plus named header set, not a global C++ prelude.
- Existing parser import forms may not preserve enough structure for this syntax. Mitigation: add an import AST classification before package graph construction.
- Fixture coverage can drift from the formal algorithm. Mitigation: keep fixture categories aligned with the documented phase names and diagnostics.

## Migration Plan

1. Add parser/elaborator support for classifying C++ namespace imports and rejecting C++ header-only imports.
2. Update the package/module graph to exclude C++ namespace imports from Cosmo module dependency edges.
3. Add the CMake target for `cosmo-clang-sys`, including Clang detection and Linux `libcosmoClang.a` artifact production.
4. Link `cosmoc` to `cosmo-clang-sys` when C++ namespace imports are enabled.
5. Add resolver binding kinds and conflict validation for foreign namespace aliases.
6. Add the formal name-resolution document and fixture corpus.
7. Extend resolver tests to read `fixtures/name-resolution` and assert positive/negative diagnostics.

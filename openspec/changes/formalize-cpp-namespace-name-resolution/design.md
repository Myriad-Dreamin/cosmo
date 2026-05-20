## Context

Cosmo currently treats imports primarily as Cosmo module dependencies, while the C++ backend already has include requirements and C++ qualified symbol machinery. A bare C++ header import such as `import "c++/vector"` does not give the resolver a bounded root symbol for paths like `std::vector`; resolving that form would either require global header search or implicit foreign symbol injection. Both behaviors make name resolution non-deterministic and create conflicts with Cosmo module names such as `std`.

The resolver should instead require an explicit local alias for a C++ namespace root. After:

```cos
import std as cstd from "c++/vector"
import std as cstd from "c++/string"
```

the leftmost symbol in `cstd::vector` is a resolved foreign namespace alias. The resolver can then continue lookup inside the merged C++ namespace import set without scanning unrelated headers or treating `std` as a Cosmo binding.

## Goals / Non-Goals

**Goals:**

- Reject `import "c++/..."` as an unsupported header-only import form.
- Support explicit C++ namespace aliases with merge semantics across multiple headers.
- Keep C++ symbols out of ordinary Cosmo lexical and module lookup unless introduced by an explicit alias.
- Define a deterministic name-resolution phase, including inputs, outputs, phase order, conflict checks, qualified lookup, and diagnostics.
- Add documentation and fixtures that make the algorithm testable.

**Non-Goals:**

- Parse every declaration from a C++ header.
- Support C++ overload resolution, ADL, concepts, macros, operators, or template substitution in this change.
- Treat `std` or any other C++ namespace as implicitly visible after a C++ import.
- Make `std::vector` resolve unless `std` itself was explicitly imported as a local alias.
- Define ABI-safe adapters between C++ standard library types and Cosmo standard types.

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

## Risks / Trade-offs

- C++ namespace imports do not prove that a referenced child symbol exists until the C++ header index or binding generator is available. Mitigation: name resolution resolves the explicit alias root and records the remaining C++ path for later C++ symbol validation.
- Users may expect `import std as cstd from "c++/vector"` to import every C++ standard library symbol. Mitigation: the spec says the import provides a namespace root plus named header set, not a global C++ prelude.
- Existing parser import forms may not preserve enough structure for this syntax. Mitigation: add an import AST classification before package graph construction.
- Fixture coverage can drift from the formal algorithm. Mitigation: keep fixture categories aligned with the documented phase names and diagnostics.

## Migration Plan

1. Add parser/elaborator support for classifying C++ namespace imports and rejecting C++ header-only imports.
2. Update the package/module graph to exclude C++ namespace imports from Cosmo module dependency edges.
3. Add resolver binding kinds and conflict validation for foreign namespace aliases.
4. Add the formal name-resolution document and fixture corpus.
5. Extend resolver tests to read `fixtures/name-resolution` and assert positive/negative diagnostics.

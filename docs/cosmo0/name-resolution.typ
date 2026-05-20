= cosmo0 Name Resolution

== Status

This file owns source name lookup, declaration binding, qualified lookup, foreign namespace aliases, and name-resolution diagnostics for the cosmo0 bootstrap subset.

== Resolver Inputs and Outputs

Name resolution consumes one already-parsed module after import elaboration has separated ordinary Cosmo imports from C++ namespace imports. Its inputs are:

- the module source span and declaration list;
- ordinary Cosmo import declarations, which may contribute package dependency edges;
- C++ namespace import declarations, which bind a local alias to a canonical C++ namespace and a bounded set of contributing headers;
- class members, function parameters, local declarations, and expression/type paths.

The resolver output is a typed binding environment containing module declarations, class/member scopes, local scopes, expression/type references, diagnostics, and the foreign namespace alias table. The foreign alias table records:

- canonical C++ namespace path, such as `::std`;
- local alias, such as `cstd`;
- contributing header names, such as `c++/vector` and `c++/string`;
- the source span of the alias declaration used for diagnostics.

== Phase Order

Resolution is deterministic and ordered:

1. collect module-level ordinary declarations and ordinary imports;
2. collect C++ namespace imports and merge compatible aliases;
3. collect class/member scopes and function parameter scopes;
4. resolve type paths, expression names, selections, variant constructors, and local declarations;
5. report unresolved, duplicate, and conflicting alias diagnostics before lowering.

Repeated input order must not change the final alias/header set except for the stable order of first contributing headers.

== Unqualified Lookup

Unqualified lookup searches the innermost local scope first and then walks outward through function, class, module, and prelude scopes. A declaration may not define the same name twice in the same scope. Duplicate ordinary bindings are reported as `cosmo1.name.duplicate-definition`.

C++ namespace imports are not implicit imports of the original C++ namespace name. After:

```cos
import std as cstd from "c++/vector"
```

`cstd` is visible, but `std` is not visible unless an ordinary Cosmo binding named `std` exists.

== Qualified Lookup

Qualified lookup resolves the leftmost segment first, then interprets the suffix by the binding kind:

- ordinary class and module-like bindings resolve members or variant constructors through the existing Cosmo scope environment;
- a foreign namespace alias resolves the remaining suffix as a C++ qualified symbol rooted at the alias's canonical namespace;
- unresolved leftmost segments report `cosmo1.name.unresolved-name`.

For example:

```cos
import std as cstd from "c++/vector"
type StdVector = cstd::vector
```

The leftmost segment `cstd` resolves to the foreign namespace alias. The suffix `vector` is interpreted as `::std::vector` and is validated against the bounded header set before backend use.

== C++ Namespace Imports

The accepted source form is:

```cos
import std as cstd from "c++/vector"
```

The namespace path before `as` is a C++ namespace path. The alias after `as` is the only name introduced into the Cosmo module scope. The string after `from` must start with `c++/` and name a header after that prefix.

Compatible repeated imports merge when both the local alias and canonical namespace match:

```cos
import std as cstd from "c++/vector"
import std as cstd from "c++/string"
```

The resulting alias is still `cstd`, targets `::std`, and contributes the header set `c++/vector,c++/string`.

The following are errors:

- `import "c++/vector"`: unsupported header-only C++ import, reported as `cosmo1.name.unsupported-cpp-header-import`;
- importing the same alias for a different namespace, reported as `cosmo1.name.conflicting-cpp-namespace-alias`;
- defining an ordinary Cosmo binding with the same name as a C++ alias in the same module, reported as `cosmo1.name.duplicate-definition`.

== Backend Validation

C++ qualified suffix validation is delegated to `cosmo-clang-sys`. The query input is the alias's canonical namespace, the requested suffix, and the bounded contributing header set. The C++ backend records both include requirements, such as `include:<vector>`, and a foreign binding requirement, such as `cpp-namespace-import:cstd=::std from c++/vector`.

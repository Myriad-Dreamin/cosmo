= cosmo0 Name Resolution

== Status

This file owns source name lookup, declaration binding, qualified lookup,
foreign namespace aliases, delayed resolution obligations, and name-resolution
diagnostics for the cosmo0 bootstrap subset.

== Resolver Inputs and Outputs

Name resolution consumes already-parsed source after import elaboration has
separated ordinary Cosmo imports from C++ namespace imports. It is incremental:
Cosmo resolves the leading segment of path-like syntax as early as possible,
then resolves later segments when the required facts become available. Its
inputs are:

- the module source span and declaration list;
- the package/module declaration index built from parsed declaration headers;
- ordinary Cosmo import declarations, which may contribute package dependency edges;
- C++ namespace import declarations, which bind a local alias to a canonical C++ namespace and a bounded set of contributing headers;
- class members, function parameters, local declarations, and expression/type paths.

The resolver output is a binding environment containing module declarations,
class/member scopes, local scopes, expression/type references, delayed
resolution obligations, diagnostics, and the foreign namespace alias table. The
foreign alias table records:

- canonical C++ namespace path, such as `::std`;
- local alias, such as `cstd`;
- contributing header names, such as `c++/vector` and `c++/string`;
- the source span of the alias declaration used for diagnostics.

== Cosmo Resolution Model

Cosmo name resolution is prefix-first and fact-driven. A path-like expression is
not required to resolve every segment in one pass. The first segment is resolved
against the current lexical, module, import, type, foreign-alias, and provider
indexes. Any remaining suffix is stored as a deterministic resolution
obligation.

For example:

```cos
value.expand(2)
```

The first resolution step only proves where `value` is bound:

```text
ResolveHead("value") -> Binding(value)
```

The selector `.expand` is not classified by string name. It is resolved after
the compiler has the facts required by selector lookup, such as the receiver
type or namespace kind:

```text
Binding(value)
  -> TypeFact(value)
  -> ResolveSelector(receiver = value, selector = "expand")
  -> ordinary method, extension method, associated item, or macro provider
```

This keeps source checking schedulable without pretending that every path can
be fully resolved during parsing. The compiler may run independent head
resolution, signature checking, provider lookup, and type-fact production tasks
in parallel, but it merges diagnostics and generated output in deterministic
source order.

== Resolution Facts And Obligations

The resolver tracks facts separately from the final typed tree:

- `NameFact`: a leading segment has a stable binding id, namespace kind,
  visibility, and source span.
- `SignatureFact`: a declaration header is known, such as function
  parameters/return type, class field signatures, or a macro provider ABI.
- `TypeFact`: an expression, local binding, or value declaration has an
  inferred or checked type.
- `ProviderFact`: a macro provider path resolves to a provider identity and
  accepted input/output shape.
- `ExecutableProviderFact`: a macro provider host artifact has been checked and
  compiled for compile-time execution.

A delayed obligation declares the facts it needs. An obligation becomes
runnable when those facts are available. This model lets `pkg.Type.member`,
`value.field`, `value.expand(2)`, and foreign namespace suffixes use the same
prefix-first scheduling model while still preserving their different lookup
rules.

== Phase Order

Resolution is deterministic and ordered by facts, not by a blind pass over
source items:

1. build a package/module declaration index from parsed declaration headers;
2. collect module-level ordinary declarations and ordinary imports;
3. collect C++ namespace imports and merge compatible aliases;
4. collect class/member scopes, function signatures, macro provider headers,
   and function parameter scopes;
5. resolve the leading segment of type paths, expression names, selector
   receivers, variant constructors, macro provider paths, and local references;
6. run delayed suffix, selector, provider, and foreign-namespace obligations
   when their required facts are available;
7. report unresolved, duplicate, conflicting alias, failed obligation, and
   unsupported-cycle diagnostics before lowering.

Repeated input order must not change the final alias/header set, diagnostic
order, generated declaration order, or macro invocation order except for stable
source-order tie-breaks.

== Unqualified Lookup

Unqualified lookup searches the innermost local scope first and then walks outward through function, class, module, and prelude scopes. A declaration may not define the same name twice in the same scope. Duplicate ordinary bindings are reported as `cosmo1.name.duplicate-definition`.

C++ namespace imports are not implicit imports of the original C++ namespace name. After:

```cos
import std as cstd from "c++/vector"
```

`cstd` is visible, but `std` is not visible unless an ordinary Cosmo binding named `std` exists.

== Qualified Lookup

Qualified lookup resolves the leftmost segment first, then interprets the suffix
by the binding kind. The suffix may be resolved immediately for namespace-like
bindings, or delayed until type/provider facts are available for term-like
bindings:

- ordinary class and module-like bindings resolve members or variant constructors through the existing Cosmo scope environment;
- a foreign namespace alias resolves the remaining suffix as a C++ qualified symbol rooted at the alias's canonical namespace;
- a term binding stores selector suffixes as obligations until the receiver type
  is known;
- a macro provider binding stores macro classification obligations until the
  selected provider path, payload shape, and provider executable facts are known;
- unresolved leftmost segments report `cosmo1.name.unresolved-name`.

For example:

```cos
import std as cstd from "c++/vector"
type CppVector[T] = cstd::vector[T]
```

The leftmost segment `cstd` resolves to the foreign namespace alias. The suffix `vector` is interpreted as `::std::vector` and is validated against the bounded header set before backend use.

== Selector And Macro Classification

Selector lookup is not textual matching. For expression syntax such as:

```cos
value.expand(2)
```

the resolver first resolves `value`. After `value` has a type fact, member,
method, and extension lookup decide what `.expand` denotes. If that resolved
target is a macro provider, macro classification runs and the provider receives
the normalized macro payload selected by the macro expression boundary. If the
resolved target is an ordinary method or value-level callable, the expression is
checked as an ordinary call.

A free `@macro def expand(...)` in scope does not by itself match every
`.expand` selector. The selected member, method, or extension lookup result must
resolve to that provider.

== Top-Level And Block-Local Scheduling

Top-level modules and declarations are planned from the package/module index and
the delayed-obligation graph. Source order is a deterministic tie-breaker, not
the semantic checking order, when independent top-level tasks can run in
parallel.

Block-local items are different. Local `val` and `var` declarations mutate the
lexical scope for later items in the same block. Cosmo therefore checks
block-local items left-to-right unless a later local dataflow planner proves a
subgroup independent without changing scope visibility, mutation, diagnostics,
or inferred types.

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

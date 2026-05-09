## Context

Cosmo currently has a Scala implementation of the full compiler and early self-hosting experiments under `packages/cosmoc/src`. The full compiler supports or experiments with features that are valuable for Cosmo but expensive for a bootstrap path: type-level values, `Type`, generic functions/classes, trait constraints, reflection, staging, and code quotation.

cosmo0 is intended to be a Scala-implemented core compiler, not a language self-hosting milestone. It should compile a deliberately small subset of Cosmo into C++ so compiler-critical components can be written in a stable subset. The key adjustment from a "no generics" subset is that compiler data structures require containers and typed IDs such as `Vec<T>`, `Option<T>`, `Map<K, V>`, `Arena<T>`, and `Id<T>`. cosmo0 therefore supports sealed standard generic type constructors while rejecting user-defined generic programming.

The cosmo1 architecture proposal makes the target concrete: cosmo0 must compile a traditional, multi-file, arena-based compiler implementation. Full Cosmo features such as `Type`, user generics, trait solving, reflection, and staging are implemented by cosmo1 as data structures and algorithms; they are not host-language features that cosmo0 must provide.

## Goals / Non-Goals

**Goals:**

- Provide a stable Scala cosmo0 compiler path for bootstrap-critical Cosmo code.
- Provide enough host-language and runtime capability to compile staged cosmo1 compiler components.
- Reuse the existing Cosmo parser where practical and enforce the cosmo0 subset after parsing.
- Support sealed, descriptor-defined standard generics such as `Vec<T>`, `Option<T>`, `Result<T, E>`, `Map<K, V>`, `Set<T>`, `Arena<T>`, `Id<T>`, `Ptr<T>`, and `Box<T>`.
- Support core0 runtime-backed facilities such as strings, string builders, JSON values, paths, filesystem operations, commands, lossless numeric literals, and deterministic collections.
- Support package-level checking/compilation and multi-module code generation for cosmo1-style packages.
- Force standard generic instances to lower to core0 registered implementations rather than user-defined alternatives.
- Keep the cosmo0 IR and type checker smaller than the full `cosmo.ir` and `Typer` pipeline.
- Make rejection of unsupported full-language constructs explicit and testable.

**Non-Goals:**

- Full self-hosting of the Cosmo compiler.
- Compiling the full `library/std` source with cosmo0.
- User-defined generic classes, functions, traits, or impls.
- General iterator traits, generic higher-order collection APIs, closures, and parser-combinator-style programming in the initial cosmo0 subset.
- Type-level computation using `Type`, dependent type normalization, HKT, trait solving, reflection, staging, macros, or quotation.
- Replacing the existing Scala full Cosmo compiler.

## Decisions

### Reuse the parser, add a cosmo0 checker

cosmo0 will parse source with the existing Cosmo parser and then run a dedicated subset checker. This avoids creating a second grammar while still allowing cosmo0 to reject features that are valid full Cosmo but invalid core code.

Alternative considered: implement a separate cosmo0 parser. That would make the accepted grammar mechanically smaller, but it creates syntax drift and doubles parser maintenance.

### Use a separate low-level cosmo0 IR

cosmo0 will lower accepted syntax into a purpose-built low-level IR rather than reusing the full `cosmo.ir` directly. The full IR carries type-level and dependent-language concerns that cosmo0 is explicitly trying to avoid.

Alternative considered: reuse the existing typed IR and disable unsupported cases. That risks retaining the same bootstrap complexity behind feature flags.

### Support sealed standard generics only

cosmo0 will support generic type applications for a closed set of standard constructors. Examples include `Vec<Node>`, `Option<Vec<Node>>`, `Result<Value, Error>`, `Map<Symbol, DefId>`, `Set<Symbol>`, `Arena<Expr>`, `Id<Expr>`, `Ptr<Node>`, and `Box<Node>`. These are type syntax and constructor targets, not first-class values.

Alternative considered: ban generics completely. That makes compiler data structures awkward and forces many artificial specialized container names.

Alternative considered: support user-defined generics. That would require generic body checking, substitution, monomorphization or template emission, method lookup over type parameters, and eventually constraints; this is outside cosmo0's purpose.

### Define standard generics through Scala descriptors

The standard generic constructors and methods will be registered in Scala descriptors. The descriptors define arity, type argument rules, constructors, method signatures, and C++ lowering. cosmo0 source cannot override these descriptors.

Alternative considered: compile `library/std/src/collections/vec.cos` with cosmo0. That source currently relies on full-language features such as `Type`, type functions, and user-defined generic class bodies, so it would pull full Cosmo complexity into cosmo0.

### Add a core0 runtime descriptor layer

cosmo1 needs more than generic containers. It needs descriptor-backed scalar and runtime facilities for strings, source text, diagnostics, JSON bridging, paths, filesystem access, command execution, numeric literals, and deterministic output. cosmo0 will expose these through core0 runtime descriptors rather than by compiling the full `library/std`.

Alternative considered: keep all runtime types in `cosmo0-std-generics`. That makes the capability name misleading and mixes generic type constructors with non-generic runtime modules such as `Fs`, `Command`, and `Json`.

### Prefer arena IDs for compiler data

cosmo1 should store AST nodes, types, scopes, definitions, and IR nodes in `Arena<T>` values and refer to them with `Id<T>`. `Id<T>` is a sealed phantom standard generic whose runtime representation can be an integer, while its source-level type prevents accidental mixing of `ExprId`, `TyId`, `DefId`, and `ScopeId`.

Alternative considered: use `Ptr<T>` throughout compiler internals. That is simple to lower to C++ but makes ownership, serialization, diagnostics, and mutation boundaries less explicit.

### Support package-level compilation early

cosmo0 must eventually check and compile a multi-file cosmo1 package, not just isolated smoke files. The initial implementation can begin with file-level commands, but the design target includes package metadata loading, module dependency ordering, namespace/name generation, runtime descriptor inclusion, and deterministic multi-module output.

Alternative considered: only support single-file bootstrap targets. That is simpler but fails to validate the main cosmo1 use case, which is a compiler split across source, lexing, syntax, package, names, types, eval, IR, codegen, link, and cache modules.

### Keep full Cosmo as the high-level path

cosmo0 is an additional core path. Full Cosmo remains responsible for experiments and high-level language features. A source file can target cosmo0 only if it passes the subset checker.

Alternative considered: migrate large parts of the full compiler to cosmo0 immediately. That creates pressure to expand cosmo0 before the subset boundary is proven.

## Risks / Trade-offs

- Feature creep -> Mitigation: unsupported constructs are rejected by spec and tests before implementation code grows around them.
- Descriptor duplication with full `library/std` -> Mitigation: start with a small core0 descriptor table and later align full std APIs deliberately.
- Parser accepts more syntax than cosmo0 -> Mitigation: all cosmo0 entry points must run the subset checker before lowering.
- `Vec<T>` support may expand into full generics -> Mitigation: only registered constructors are valid, and user-defined generic parameters remain invalid in cosmo0 source.
- Runtime API mismatch with C++ containers -> Mitigation: prefer a core0 wrapper API in descriptors so source-level `Vec<T>` does not expose arbitrary STL details.
- cosmo1 pressure expands cosmo0 into full Cosmo -> Mitigation: require full Cosmo features to be represented as cosmo1 compiler data and explicitly reject host-language `Type`, user generics, traits, reflection, and staging.
- Insufficient runtime support blocks cosmo1 stages -> Mitigation: classify core0 runtime requirements by stage and implement only the first stage's required descriptors first.
- Non-deterministic map iteration destabilizes snapshots and generated code -> Mitigation: make `Map<K, V>` deterministic by default or require an explicit deterministic map for output-affecting state.

## Migration Plan

1. Add cosmo0 specs, design, and tasks.
2. Implement the subset checker and descriptor registry without changing full Cosmo behavior.
3. Add core0 runtime descriptors needed for Stage 1 cosmo1 components: scalar/string support, `Vec<T>`, `Option<T>`, `Result<T, E>`, `StringBuilder`, spans/diagnostic-friendly operations, and deterministic output support.
4. Add a small cosmo0 pipeline and CLI/test entry point.
5. Expand the descriptor registry for `Arena<T>`, `Id<T>`, `Map<K, V>`, `Set<T>`, `JsonValue`, and package-level support as cosmo1 stages require them.
6. Validate by compiling staged cosmo1 smoke targets to C++ and running smoke tests against small Cosmo inputs.

Rollback is straightforward while cosmo0 remains additive: remove or disable the cosmo0 entry point and leave the full compiler path unchanged.

## Open Questions

- Which source syntax should be canonical for generic type application in cosmo0: existing Cosmo-style `Vec(T)`, bracket style `Vec[T]`, or a future `Vec<T>` spelling?
- Should `Ptr<T>` and `Ref<T>` be modeled as generic constructors in the descriptor table or as primitive type forms?
- Should cosmo0 initially emit direct STL types or core0 wrapper types such as `cosmo0::Vec<T>`?
- Should `Span`, `Diagnostic`, `Symbol`, `Arena<T>`, and `Id<T>` live in core0 runtime descriptors, cosmo1 modules, or split between both?
- Should `Map<K, V>` be deterministic by default, or should deterministic behavior be exposed through a separate `OrderedMap<K, V>`?
- Should Stage 1 numeric literals be represented as raw text, `BigInt`, or both?
- Which cosmo1 stage should be the first acceptance target: diagnostics/lexer, JSON AST resolver, parser front-end, package loader, or C++ emission smoke?

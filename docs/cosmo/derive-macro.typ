= cosmo0 Derive Macros

== Status

This file owns the first cosmo0 derive macro design. It is intentionally
narrower than general declaration macros and expression macros. The initial
derive boundary only allows a provider to attach implementations of existing
traits to an existing item. It does not introduce new public names, new members,
new top-level declarations, or parser syntax.

== Derive Boundary

A derive macro is selected from an attribute on an existing item:

```cos
@derive(cli.Parser)
class Config {
  val package_name: String
}
```

The attribute identifies a derive provider. The target item already exists in
the parsed declaration index before the provider runs. The provider receives
compiler-selected reflection facts for that item and may return one or more
trait implementation records for the same item.

Accepted first-slice output shape:

```text
DeriveOutput:
  impl trait cli.Parser for Config { ... }
```

Rejected first-slice output shapes:

```text
new top-level function parse(...)
new static method Config.parse(...)
new field or variant on Config
new type alias or class
raw source text
trusted TypedExpr
```

This keeps derive expansion an implementation-attachment phase, not a
declaration-introduction phase.

== Name Resolution Boundary

Derive macros do not affect ordinary name resolution in the first slice.

The resolver builds the package/module declaration index and resolves ordinary
names before derive output is needed. Because derive output can only attach an
implementation of an already-known trait to an already-known item, expansion
does not add a new binding that later source can refer to by name.

For example:

```cos
@derive(cli.Parser)
class Config { ... }
```

may make `Config` satisfy trait `cli.Parser`, but it does not create a visible
`parse` name, a `Config.parse` member, or a new module declaration. Later code
that uses parser behavior must do so through trait resolution or an explicit
trait API that was already name-resolved:

```cos
val config = cli.Parser.parse[Config](args)
```

In this shape:

- `cli.Parser` is resolved as an ordinary trait/API path before derive output
  is attached;
- `Config` is resolved as an ordinary class before derive output is attached;
- the derive macro only supplies the implementation evidence connecting them.

== Provider Input

The compiler supplies stable derive input selected from parsed declarations and
admitted type facts:

```text
DeriveInput:
  provider identity
  source package identity
  target item identity
  target item kind
  target item name, module path, visibility, and source span
  admitted type parameters
  fields, variants, defaults, attributes, and doc comments admitted by profile
  selected trait identity
  source spans and hygiene/origin metadata
```

The provider does not receive a mutable compiler module, a typed tree to patch,
or a target runtime executable handle.

== Provider Output

The provider returns serialized output:

```text
DeriveOutput:
  trait implementation records
  consumed attributes
  diagnostics
  generated-source summary
```

Each implementation record must name:

- the already-resolved trait being implemented;
- the already-resolved target item;
- generated method bodies or associated implementation data admitted by the
  trait contract;
- generated spans and origin spans.

The compiler validates the output before attaching it. Generated expression
fragments inside implementation bodies are `Expr[Untyped]` and return to
ordinary type checking.

== Trait Implementation Attachment

Attaching a derive implementation is semantically equivalent to accepting an
ordinary implementation block for the same trait and target, except that the
implementation body is generated and carries derive origin metadata.

The attachment phase validates:

- the trait exists and is admitted by the derive provider;
- the target item exists and is a supported item kind;
- the generated implementation satisfies the trait's required members;
- the same trait is not implemented twice for the same target in a conflicting
  way;
- all provider-consumed attributes are recorded;
- unconsumed macro-owned attributes are diagnosed.

Because the output is an implementation attachment, not a new declaration, the
ordinary declaration index is unchanged by derive expansion.

== Trait Resolution Dependency

Derive output may affect trait resolution even though it does not affect
ordinary name resolution. A generated implementation contributes an
implementation fact:

```text
ImplFact(trait = cli.Parser, target = Config)
```

The implementation fact index is built from both source impl declarations and
derive-generated implementation attachments:

```text
ImplFactIndex =
  source impls
  + derive-generated impls
```

Type checking of an item that requires trait evidence depends on the relevant
implementation fact. For example:

```cos
val config = cli.Parser.parse[Config](args)
```

uses ordinary name resolution for `cli.Parser` and `Config`, but it cannot
finish trait resolution until `ImplFact(cli.Parser for Config)` is available.
If a method-like trait API such as `config.parse()` is admitted, selector
resolution must also wait for the method-set fact that can include
derive-generated impls:

```text
ResolveSelector(config.parse)
  waits for TypeFact(config)
  waits for MethodSetFact(Config, "parse")
```

The first derive slice keeps derive input smaller than this output dependency:
derive expansion may depend on declaration header facts, selected trait
identity, target item identity, fields, variants, attributes, defaults, doc
comments, and admitted type facts. It must not depend on arbitrary body checking
or trait-resolution facts unless a later inspector capability adds explicit
dependency edges.

== Diagnostics

Derive diagnostics must point both to the derive attribute and to the target
item fact that caused the issue. Generated implementation diagnostics must carry
the generated span and the origin span from the derive input.

Examples:

```text
cosmo0.derive.unresolved-provider
cosmo0.derive.unsupported-target
cosmo0.derive.unsupported-trait
cosmo0.derive.invalid-output
cosmo0.derive.duplicate-impl
cosmo0.derive.unconsumed-attribute
```

== Non-Goals

The first derive macro slice does not provide:

- new public declarations;
- new members on the target item;
- arbitrary generated classes, aliases, fields, or variants;
- expression macro calls;
- parser plugins or token-tree macros;
- self-hosted provider package loading;
- target runtime execution during expansion.

Later declaration macro capabilities may introduce new names, but they must be
specified separately because they do affect name resolution and declaration
ordering.

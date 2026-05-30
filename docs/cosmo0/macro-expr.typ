= cosmo0 Macro Expressions

== Status

This file owns the macro expression value boundary for cosmo0 macro design. It
is a design-facing specification page: the default cosmo0 subset still rejects
ordinary macros until an accepted capability admits them, but future macro work
must use this boundary instead of reintroducing a broad compiler-internal
expression API.

The central rule is that public macro expression values are written as
`Expr[T = Untyped]`. In the current contract, `Untyped` is the only stable
expression type argument. It is a macro-level phase marker meaning "not yet
checked by the ordinary typer"; it is not an object-language runtime type, not
the top type, and not the compiler's internal `UntypedExpr`.

== Expression Value Boundary

`Expr[Untyped]` is a structured source-expression value that a macro provider
may consume, splice, forward, and produce. It carries source span and
hygiene/origin metadata, but it is not a trusted checked expression.

The compiler keeps these concepts separate:

- `SourceExpr`: compiler-internal source expression data.
- `Expr[Untyped]`: provider-facing untyped source expression value.
- `AttrExpr`: restricted attribute argument syntax.
- `ConstValue`: evaluated compile-time data.
- `TypedExpr`: ordinary type-checker artifact, never provider-constructible.
- typed inspector output: read-only facts returned by typer-phase inspectors.

Providers must not treat the `T` parameter of `Expr[T = Untyped]` as evidence
for an arbitrary object-language result type. Typed facts are requested through
inspectors such as `Type.of(expr)`, and inspector output does not authorize a
provider to mark an expression as typed.

== Expression Macro Protocol

Expression macros use ordinary Cosmo call syntax. The parser first accepts a
normal source expression; name resolution decides whether the callee is a
compile-time provider or an ordinary function.

The first protocol shape is:

```text
ExprMacroInput:
  path: PathRef
  args: List[Expr[Untyped]]
  callSpan: SourceSpan

ExprMacroOutput:
  expr: Expr[Untyped]
  diagnostics: List[MacroDiagnostic]
```

Generated expression output always returns to ordinary type checking. A provider
may generate a candidate expression, but it cannot manufacture or inject a
trusted `TypedExpr`.

== Typed Inspection

Typed macro information is exposed through bounded typer inspectors, not through
a typed expression tree. A representative inspector is:

```text
Type.of(expr): TypeInfo
```

The input `expr` remains `Expr[Untyped]`. The result is a stable type fact for
that expression as observed by the ordinary typer. It is not a mutable
type-checker node, not a scope resolver handle, and not permission to bypass
rechecking.

== Non-Goals

This boundary intentionally does not admit:

- multi-stage typed quotation;
- `Expr[Any]` as a synonym for untyped code;
- `Expr[i32]` as proof that the expression has type `i32`;
- provider construction of `TypedExpr`;
- raw token-tree macros or parser plugins;
- raw source text as primary generated output;
- direct patching of typed modules, lowering IR, or backend state.

== Examples

Accepted macro expression shape once expression macros are admitted:

```cos
val xs = vec(1, 2, 3)
```

Provider-level sketch:

```text
vec:
  List[Expr[Untyped]] -> MacroResult[Expr[Untyped]]

Type.of(arg0): TypeInfo
```

Rejected provider API shapes:

```text
List[Expr[Any]] -> Expr[Any]
List[Expr[i32]] -> Expr[Vec[i32]]
TypedExpr -> TypedExpr
TokenStream -> TokenStream
```

The rejected examples either confuse untyped source expressions with an
object-language top type, imply typed quotation, expose trusted checked trees,
or bypass the Cosmo parser.

== Review Rules

Macro expression proposals must preserve these rules:

- `Expr[Untyped]` remains untyped until ordinary type checking accepts it.
- `Untyped` remains a phase marker, not a runtime type.
- typed facts come from inspectors such as `Type.of(expr)`.
- inspector output is read-only and cannot be used to mark generated code as
  already checked.
- generated expression output must be deterministic and must re-enter ordinary
  checking before lowering.

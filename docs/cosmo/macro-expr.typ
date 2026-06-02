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

This page only defines the expression value boundary. At that boundary, the
stable provider-facing expression value is `Expr[Untyped]`.

Providers must not treat the `T` parameter of `Expr[T = Untyped]` as evidence
for an arbitrary object-language result type. Typed facts are requested through
inspectors such as `Type.of(expr)`, and inspector output does not authorize a
provider to mark an expression as typed.

== Expression Macro Function Contract

Expression macros are macro functions selected from already-parsed source
expressions. The surface syntax does not have to be a function call. A call
expression, method-like application, interpolation, block-attached application,
or another syntax form admitted by a future capability may all select an
expression macro after parsing and name resolution.

The parser first accepts ordinary source syntax. Name resolution and macro
classification then decide whether the parsed expression targets a compile-time
provider or an ordinary value-level operation. Macro selection must not depend
on exposing a raw parser token stream to the provider.

Provider-facing expression macro behavior is specified as a macro function
implementation shape:

```text
macro def provider(input: Expr[Untyped]): Expr[Untyped] = {
  val generated: Expr[Untyped] = ...
  return generated
}
```

The provider-facing input is the selected source expression as `Expr[Untyped]`.
The compiler may still record provider identity, source spans, hygiene/origin
metadata, diagnostics, and execution context in the surrounding macro execution
record, but those are not a second public provider argument. Call syntax is one
possible `Expr[Untyped]` shape, not the general macro ABI.

Generated expression output always returns to ordinary type checking. A provider
may generate a candidate expression, but it cannot manufacture or inject a
trusted checked expression object.

== Macro Input Shapes

Different surface forms feed different `Expr[Untyped]` shapes to the macro
function. The shape names below are illustrative; the stable rule is that the
provider receives one parsed expression value, not raw tokens and not a special
context value.

Call-like syntax feeds the whole call expression:

```cos
val xs = vec(1, 2, 3)
```

```text
macro def vec(input: Expr[Untyped]): Expr[Untyped]

input =
  Expr.Call(
    callee = Expr.Name("vec"),
    args = [Expr.Int(1), Expr.Int(2), Expr.Int(3)],
  )
```

Method-like syntax feeds the selected expression shape, preserving the receiver
inside the expression:

```cos
val y = value.expand(2)
```

```text
macro def expand(input: Expr[Untyped]): Expr[Untyped]

input =
  Expr.Call(
    callee = Expr.Select(Expr.Name("value"), "expand"),
    args = [Expr.Int(2)],
  )
```

Block-attached syntax feeds one expression whose target and block body are both
part of the parsed expression:

```cos
val field = j.path { self.field(0) }
```

```text
macro def path(input: Expr[Untyped]): Expr[Untyped]

input =
  Expr.BlockApply(
    target = Expr.Select(Expr.Name("j"), "path"),
    body = Expr.Block([
      Expr.Call(
        callee = Expr.Select(Expr.Name("self"), "field"),
        args = [Expr.Int(0)],
      ),
    ]),
  )
```

Interpolation feeds one interpolation expression. Literal parts are structured
data inside that expression node, while holes remain `Expr[Untyped]` values:

```cos
val message = fmt"x $name y"
```

```text
macro def fmt(input: Expr[Untyped]): Expr[Untyped]

input =
  Expr.Interpolate(
    tag = Expr.Name("fmt"),
    parts = [
      Text("x "),
      Hole(Expr.Name("name")),
      Text(" y"),
    ],
  )
```

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
- provider construction of already-checked expression objects;
- raw token-tree macros or parser plugins;
- raw source text as primary generated output;
- direct patching of typed modules, lowering IR, or backend state.

== Examples

Accepted macro function implementation shape once expression macros are
admitted. Helper method names on `Expr[Untyped]` are illustrative:

```text
macro def vec(input: Expr[Untyped]): Expr[Untyped] = {
  val items = input.callArgs()
  val output = quote_expr(Vec.from_array(Array(..items)))
  return output
}
```

Call-like use is still allowed, but it is only one accepted surface form:

```cos
val xs = vec(1, 2, 3)
```

The same boundary can describe a block-attached application. The macro inspects
the input expression to get the target and block body; the body is structured
`Expr[Untyped]` data, not raw source text and not a checked expression:

```cos
val field = j.path { self.field(0) }
```

```text
macro def path(input: Expr[Untyped]): Expr[Untyped] = {
  val receiver = input.blockTarget()
  val body = input.blockBody()
  val output = build_path_access(receiver, body)
  return output
}
```

Interpolation may also be admitted without making the source expression look
like an ordinary call:

```cos
val message = fmt"x $name y"
```

```text
macro def fmt(input: Expr[Untyped]): Expr[Untyped] = {
  val parts = input.interpolationParts()
  val output = build_format_expr(parts)
  return output
}
```

Rejected provider API shapes:

```text
Expr[Any] -> Expr[Any]
Expr[i32] -> Expr[Vec[i32]]
List[Expr[Untyped]] -> Expr[Untyped]
Call(path: PathRef, args: List[Expr[Untyped]]) -> Expr[Untyped]
TokenStream -> TokenStream
```

The rejected examples either confuse untyped source expressions with an
object-language top type, imply typed quotation, claim checked output, add a
separate public context value, force macros into a pre-extracted argument-list
or function-call shape, or bypass the Cosmo parser.

== Review Rules

Macro expression proposals must preserve these rules:

- `Expr[Untyped]` remains untyped until ordinary type checking accepts it.
- `Untyped` remains a phase marker, not a runtime type.
- typed facts come from inspectors such as `Type.of(expr)`.
- inspector output is read-only and cannot be used to mark generated code as
  already checked.
- expression macro provider input is `Expr[Untyped]`, not a separate context
  value, raw token stream, or pre-extracted argument list.
- generated expression output must be deterministic and must re-enter ordinary
  checking before lowering.

= cosmo0 Expressions

== Status

This file owns expression typing and expression validity for cosmo0. The headings are the initial skeleton; later capability changes fill the rules.

== Literals

Placeholder for accepted unit, boolean, integer, numeric-text, string, byte, char, and aggregate literal behavior.

== Examples

Accepted expression shapes:

```cos
val at_end: Bool = cursor.offset == source.len()
val first: Option<Char> = source.get(0)
val token = Token(TokenKind.Ident, span, "name")

val tag = token.kind match {
  case TokenKind.Eof => 0
  case _ => 1
}
```

Rejected expression shapes until later specs admit them:

```cos
val inc = (x: i32) => x + 1
val quoted = quote { cursor.offset }
val lifted = Type(1)
```

The accepted example uses literals, selection, calls, construction, and a simple match. The rejected example uses a lambda, quotation, and type-level lifting.

== Names and Selection

Placeholder for local names, fields, methods, module-qualified names, standard API names, and rejected lookup forms.

== Calls and Construction

Placeholder for function calls, method calls, constructors, variant construction, standard API calls, and call diagnostics.

== Assignment and Mutation

Placeholder for local assignment, field assignment, mutable reference updates, and mutable standard API operations.

== Blocks

Placeholder for block expression typing, sequencing, statement results, local scope, and return-value rules.

== Matches

Placeholder for match expressions over primitive values and supported variants, including wildcard handling and later exhaustiveness policy.

== Rejected Expressions

Placeholder for lambdas, closures, type-level expressions, macros, quotation, reflection, staging, and other full-language expressions outside cosmo0.

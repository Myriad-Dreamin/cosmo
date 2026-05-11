= cosmo0 Classes and Declarations

== Status

This file owns declaration forms accepted by cosmo0. The headings define review ownership; detailed declaration semantics are placeholders for later changes.

== Class Subset

Placeholder for non-generic class declarations, class bodies, member ordering, visibility, and the restrictions that keep cosmo0 classes suitable for bootstrap compilation.

== Examples

Accepted declaration shapes:

```cos
class Diagnostic {
  val span: Span
  val message: String
}

class TokenKind {
  case Ident
  case Number
  case Eof
}

class Cursor {
  var offset: usize

  def advance(&mut self): Unit = {
    self.offset = self.offset + 1
  }
}
```

Rejected declaration shapes until later specs admit them:

```cos
trait Render {
  def render(&self): String
}

class Pair[L, R] {
  val left: L
  val right: R
}
```

The accepted example shows concrete classes, enum-style variants, a mutable field, and a mutable receiver. The rejected example shows trait and generic class declarations outside the initial subset.

== Fields and Methods

Placeholder for field declarations, method signatures, receiver forms, method bodies, and mutation permissions.

== Constructors

Placeholder for constructor availability, construction syntax, field initialization, and generated or descriptor-backed construction behavior.

== Variants

Placeholder for enum-style case variants, variant payloads, construction, tag checks, and matching support used by the bootstrap subset.

== Imports and Visibility

Placeholder for imports, public declarations, local names, package-level visibility, and any limits on importing implementation-only modules.

== Rejected Declarations

Placeholder for generic classes, generic functions, traits, impls, macros, decorators, and any other declaration forms not accepted by cosmo0.

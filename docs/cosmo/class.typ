#import "mod.typ": *

#show: book-page.with(title: "Class")

Besides primary usage, the following type features are notably uniformed into class syntax:

== Enum Class (ADT)

```cos
class Nat {
  case Zero
  case Succ(Nat)
}
```

== Phantom Class (Phantom Type)

These classes has no size at runtime.

```cos
class Nop {}
class Identity(T: Type) {}
```

Therefore, you could get an "runtime instance" without construction:

```cos
val nop = Nop;
// is equivalent to
val nop = Nop();
```

// Passing such an instance of type to a function will not take any register or stack space.

// ```cos
// def f(implicit Ty: Type, a: Int, b: Int, tag: Ty) = {}
// f(1, 2, Nop);
// // uses same registers and stack space as
// f(1, 2);
// ```

// Boxing such an instance of type will allocate memory of size 1, though.

== Generalized Enum Class (GADT)

An optional return type can be specified for each case. if not specified, a case will be completed with `Self` as the return type.

```cos
class Expr(R: Type) {
  case Cmp(Expr(R), Op, Expr(R)): Expr(bool)
  case IntAdd(Expr(R), Expr(R)): Expr(i64)
  case CanoAdd(implicit Ty: typeof R, Expr(Ty), Expr(Ty)): Expr(Ty)
}
type UntypedExpr = Expr(i64 | bool);
```

This is called GADT formally.

== Generalized Trait (Type Class)

trait is a special kind of class that you don't need to implement all the methods.

Example:

```cos
trait Monad(M: Type => Type) {
  def lift(a: X): M(X)
  def compose(X: Type, Y: Type, M(X), M(Y)): M(Y)
  def flatMap(X: Type, Y: Type, x: M(X), f: Fn(X, M(Y))): M(Y)
}
```

Then we can implement it for ```cos Maybe: Type => Type```:

```cos
class Maybe(T: Type) {
  case Just(T)
  case Nothing
}

impl(X: Type) Monad(Maybe(X)) for Maybe(X) {
  val lift = Maybe.Just;
  val compose(x, y) = x or y;
  def flatMap(x, f) = x match {
    case Maybe.Just(a) => f(a);
    case Maybe.Nothing => Maybe.Nothing;
  }
}
```

There is still several problems on thinking about trait.

Design Question 1: First, ```cos impl Monad(Maybe(X)) for Maybe(X)``` looks mysterious.

Second, considering we rewrite `x` in the `Monad` trait with `self`.

```cos
trait MonadSelf(X: Type): Self(X) {
  def lift(a: X): Self(X)
  def compose(Y: Type, self: Self(X), M(Y)): M(Y)
  def flatMap(Y: Type, self: Self(X), f: Fn(X, M(Y))): M(Y)
}
impl(X: Type, T: Type => Type, T(X) <: Monad(T(X))) MonadSelf(X) for T(X) {
  val lift = Maybe.Just;
  val compose(self, y) = self or y;
  def flatMap(self, f) = Monad.flatMap(self, f);
}
```

Design Question 2: how could we unify implementation of `Monad` and `MonadSelf`?

== Deriving Trait (Type Class)

```cos
trait Eq(X: Type, Y: Type = X) {
  def eq(x: X, y: Y): bool = not neq(x, y)
  def neq(x: X, y: Y): bool = not eq(x, y)
}
```

Design Question 3: `Y: Type = X` looks mysterious.

Design Question 4: Either `eq` or `neq` must be implemented. How do we safely check this at runtime?

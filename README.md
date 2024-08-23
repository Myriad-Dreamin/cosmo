# Cosmo

Cosmo is a language replacing awful C++'s compile-time magics. This is only some thinking about getting rid of C++ under the fact that we cannot get rid of using C++ libraries.

## What's basic idea?

It sees cosmo functions and evaluate the type parts in the functions. The resulting functions is simply generated to C++ code.

## Example

```scala
def Vec(Ty: Type) = std.cpp.ty(std.vector(Ty));

def main() = {
  val vec = Vec(u8)();
  vec.push_back(1);
  vec.push_back(2);
  vec.push_back(3);
  println(vec.size());
}
```

Output:

```
3
```

## Build and Run

Requirements:

- scala 3.3.3
- sbt
- any clang supporting C++17

```
yarn compile && node cmd/cosmo/main.js samples/HelloWorld/main.cos samples/HelloWorld/main.cc
clang -std=c++17 samples/HelloWorld/main.cc -o test && test
```

## Syntax

External types can be handled by builtin `external` function:

```scala
def Vec(Ty: Type) = std.cpp.ty(std.vector(Ty));
```

Function body can be a type:

```scala
def Source /* inferred as : Type */ = class {
  val data = Vec(u8)
}
def Pair(Lhs: Type, Rhs: Type) /* inferred as : Type */ = (Lhs, Rhs);
```

Lifted values must be known and evaluated at compile-time

```scala
val lift(implicit T: Type)(val v: T) = Type;
val True = lift(true);
// or
val False = Type(false);
```

Traits are classes containing unimplemented methods, while you can provide default impls:

```scala
trait Unsigned(T: Type) {
  def asUint64(self): u64 = staticCast(u64)(self);
}
```

Constraints are compile-time assertions containing type expressions:

```scala
trait Unsigned(T: Type) {
  assert(T == u8 or T == u16 or T == u32 or T == u64);
}
```

The `Truthy` predicate can convert any type to either `True` or `False`:

```scala
def IsUnsigned(T: Type) = {
  assert(Truthy(Unsigned(T)));
  True
}
// or
def IsUnsigned(T: Unsigned(T)) = True;
```

Constructing a type from a type expression:

```scala
def RoundBits(T: Type) = if (IsUnsigned(T)) {
  u64
} else {
  i64
}
```

Enum and pattern matching:

```scala
// Enum class will be translated into tagged union
class Nat {
  case Zero
  case Succ(Nat)
}

def add(A: Nat, B: Nat): Nat = A match {
  case Zero => B
  case Succ(B) => Succ(Add(A, B))
}
```

Inference and specialization:

```scala
// implicit type parameters are inferred automatically
val identity(implicit T: Type)(val v: T) = v;
// Since they are currying functions, grouped braces are not necessary
val identity(implicit T: Type, val v: T) = v;

// partial specialization
val identityU8 = identity(implicit u8);
```

## Semantics

The runtime behavior depends on C++.

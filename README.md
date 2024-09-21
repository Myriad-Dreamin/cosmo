# Cosmo

Cosmo is a language replacing awful C++'s compile-time magics. This is only some thinking about getting rid of C++ under the fact that we cannot get rid of using C++ libraries.

## What's basic idea?

It sees cosmo functions and evaluate the type parts in the functions. The resulting functions is simply generated to C++ code.

## Example

```scala
import "@lib/c++/vector";

// A function returning a type
def CppVec[T]: Type = cstd.vector(T);

def main() = {
  val vec = CppVec(u8)();
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
- C++ compiler supporting C++17
  - MSVC, Clang, or GCC

```
yarn compile && node cmd/cosmo/main.js run samples/HelloWorld/main.cos
```

## Implementation Note

Demonstration:

- Literals
  - [x] Integer
  - [x] Float
  - [x] Boolean
  - [x] String
- Compound Literals
  - [ ] Template Literals
  - [x] Char
  - [x] Bytes
  - [x] Byte
  - [x] Array
  - [x] Dict
  - [ ] Lambda
- Declarations and Statements
  - [x] Variable
  - [x] Function
  - [x] Class
  - [x] Enum Class
  - [x] Trait
  - [x] Impl
  - [x] Import
- Expressions
  - [x] Binary
  - [x] Unary
  - [ ] As
  - [x] Match
  - [x] If
  - [x] For
  - [x] While
  - [x] Loop
  - [x] Break/Continue/Return
  - [x] Block
- Decorators/Macros
  - [x] Decorator
  - [ ] Macro

Value Semantics:

- Literals
  - [x] TodoLit
  - [x] BoolLit
  - [ ] IntLit
    - [x] i32
    - [ ] bigint
  - [ ] FloatLit
  - [x] StringLit
  - [x] SelfVal, SelfTy
  - [x] Identifier
  - [ ] argsLit
- Control Flow
  - [ ] block
  - [x] Loop
  - [x] While
  - [x] For
  - [x] Break
  - [x] Continue
  - [x] Return
  - [x] If
- Operations
  - [ ] valueExpr
  - [ ] typeExpr
  - [ ] unOp
    - [ ] RefMut
    - [ ] Ref
    - [ ] Mut
    - [ ] deref
  - [ ] binOp
    - [x] select
    - [ ] asExpr
    - [ ] matchExpr
    - [ ] apply
      - [ ] applyCTypes
      - [ ] applyFunc
      - [ ] applyClass
      - [ ] applyType
      - [ ] applyTemplate
    - [ ] keyedPair
    - [ ] decorate
- Declarations
  - [ ] import
  - [x] varItem
  - [x] defItem
  - [x] classItem
  - [ ] implItem

Type Operations:

- [ ] associateImpl
- [ ] cast
  - [ ] castArgs
  - [ ] castTo
- [ ] eval
- [ ] lift
- [ ] coerce
- [ ] normalize
- [ ] isSubtype

Type Guards:

- [ ] checkedMut

## Documentation

See [Design Docs.](https://myriad-dreamin.github.io/cosmo/)

## Syntax: Function

External types can be handled by builtin `external` function:

```scala
def CppVec(T: Type): Type = cstd.vector(T);
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
def lift(implicit T: Type)(v: T) = Type(v);
val True = lift(true);
// or
val False = Type(false);
```

The signature of `lift` looks a bit unfamiliar, but when we _rewrite_ this with constraint list syntax, it looks like this:

```scala
def lift[T](v: T) = Type(v);
```

Or simply as:

```scala
type lift = Type;
```

This tells us the "template arguments" in Cosmo, such as `[T]`, are the first arguments. The `T` is the first parameter of `lift` and has the type `Type`, which is the type of all cosmo types of values. When parameters are given, the cosmo compiler will evaluate these parameters at compile time and generate remaining part as a runtime function. In particular, functions whose parameters are all types, like `lift`, will be evaluated at compile time completely.

## Syntax: Trait

Traits are classes containing unimplemented methods, while you can provide default impls:

```scala
trait Unsigned[T] {
  def asUint64(&self): u64 = staticCast(u64)(self);
}
```

Constraints are compile-time assertions containing type expressions:

```scala
trait Unsigned[T] {
  assert(T == u8 or T == u16 or T == u32 or T == u64);
  def asUint64(&self): u64 = staticCast(u64)(self);
}
```

You can also put constraints inside of the constraint list:

```scala
trait Unsigned[T, T == u8 or T == u16 or T == u32 or T == u64] {}
```

or simply as:

```scala
trait Unsigned[T <: u8 | u16 | u32 | u64] {}
```

## Syntax: Higher-Kinded Types

Constructing a type from a type expression:

```scala
def RoundBits[T] = if (IsUnsigned(T)) {
  u64
} else {
  i64
}
```

## Syntax: ADT and Pattern Matching

Enum classes and pattern matching share a same syntax:

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

GADT is also supported:

```scala
class VecGADT[n: u32, T] {
  case Nil: VecGADT[0, T]
  case Cons(T, VecGADT[n - 1, T]): VecGADT[n, T]
}

impl[n: u32, T] VecGADT[n, T] {
  def concat[m: u32](self, v: VecGADT[m, T]): VecGADT[n + m, T] = self match {
    case Nil => v
    case Cons(h, t /* n - 1 */) => Cons(h, t.concat(v) /* n - 1 + m */) // n + m
  }
}
```

## Semantics

The runtime behavior depends on C++.

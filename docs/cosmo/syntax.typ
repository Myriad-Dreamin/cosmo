#import "mod.typ": *

#show: book-page.with(title: "Syntax")

// #set page(height: auto)

#show raw.where(lang: "cpp", block: true): set block(above: 0.5em)

= #text(1.4em)[Cosmo Syntax Design]

#let divider = context line(length: 109% - 2.5cm / 2, stroke: 0.5pt + text.fill)
#let part(content) = [
  #pagebreak()
  = #content
  #divider
]

Principles:

- Minimal Syntax Base
  - The basic syntax is just `var`, `def`, `loop` & `break`, and `class`. The rest of all syntax are either syntax variants or sugar of "operational trait".
- Composition-first polymorphism.
  - no inheritance among classes.
  - a class must implement a trait only by C++ concept.
  - simple subtyping: arrays, dictionaries, traits and functions derives subtyping simply by their members.
  - `val` and `import` aliasing. You can steal methods from otherwhere simply by aliasing:
  ```cos
  class/trait T {
    import (method) from OtherTrait;
    val method = OtherTrait.method;
  }
  ```
- Consistent syntax.
  - Doesn't distinguish macro (pre-compile-time) functions, type (compile-time) functions and value (runtime) functions.
  - Consistent enum/pattern matcing syntax:
  ```cos
  class Option {
    case None
    case Some(T)
  }
  opt match {
    case None => ..,
    case Some(_) => ..,
  }
  def checkOption(t: Option(T)) = /* could omit t match */ {
    case None => ..,
    case Some(_) => ..,
  }
  ```
  - Consistent declaring syntax:
  ```cos
  val Pair = False;
  type Pair = False;
  def Pair = False;

  val Pair = (U, V) => (U, V);
  type Pair(U, V) = (U, V);
  def Pair(U, V) = (U, V);

  def Pair(U, V) = class { val first: U; val second: V; };
  class Pair(U, V) { val first: U; val second: V; };
  ```
  - Consistent constructing syntax among array, dict, and arguments.
  ```cos
  val arr = (1, 2, 3);
  val dict = (key: "value");
  val someCall = someFunc(1, 2, 3, key: "value");
  ```
  - Consistent applying syntax among destructing, function calls, and import.
  ```cos
  val (a, b: c) = Args(1, b: "2");
  someCall(1, b: "2");
  import (a, b: c) from std.some.mod;
  ```
- Modular design and C++ interoperability.
  - visibililty for faster compilation and safe API header.
  - painlessly reusing any C++ code by `auto` magics.
  - C++ side code can use cosmo functions by imcluding API headers.

#part[ Part 1: Functions]

== Functions

External types can be handled by builtin `external` function:

```cos
def Vec(Ty: Type) = external(cpp, std.vector(Ty));
```

```cpp
template <typename Ty>
using Vec = std::vector<Ty>;
```

Function body can be a type:

```cos
def Source /* inferred as : Type */ = class {
  val data = Vec(u8)
}
def Pair(Lhs: Type, Rhs: Type) /* inferred as : Type */ = (Lhs, Rhs);
```

```cpp
class Source {
  Vec<u8> data;
};

template <typename Lhs, typename Rhs>
using Pair = std::tuple<Lhs, Rhs>;
```

== Implicit Parameters

Inference and specialization for implicit parameters:

```cos
// implicit type parameters are inferred automatically
val identity(implicit T: Type)(val v: T) = v;
// Since they are currying functions, grouped braces are not necessary
val identity(implicit T: Type, val v: T) = v;

// partial specialization
val identityU8 = identity(implicit u8);
```

```cpp
template <typename T>
T identity(T v) {
  return v;
}

constexpr auto identityU8 = identity<u8>;
```

== Type Level

Values are types at level 0. Types are types at level 1. Types at higher levels are usually constructed by functions.

In particular, constant values can be lifted and evaluated at compile-time:

```cos
val lift(implicit T: Type)(val v: T) = Type;
val True = lift(true);
// or
val False = Type(false);
```

```cpp
template <typename T>
struct lift {
  using type = T;
};

using True = std::true_type;
using False = std::false_type;
```

== Dependent Types

Constructing a type from a type expression:

```cos
def RoundBits(T: Type) = if (IsUnsigned(T)) {
  u64
} else {
  i64
}
```

```cpp
template <typename T>
using RoundBits = std::conditional_t<IsUnsigned<T>::value, u64, i64>;
```

== Hygiene Macros

You can view the syntax values as having negative level, but we don't model them with that sense. The hygiene macros are just evaluated before any other expressions and cannot be evaluated in latter stages:

```cos
def path(implicit S: syntax.Expr, self, field: S) = S match {
  case syntax.Expr.Apply(syntax.Expr.Self, field) | syntax.Expr.Ident(field) => std.code {
    self.apply(field)
  }
  case syntax.Expr.Apply(field, index) => std.code {
    self.apply(field).andThen(_.value.apply(index))
  }
  case syntax.Expr.Select(target, field) => std.code {
    self.path(target)
  }
  case _ => panic("path is not valid");
}
```

```cpp
/* no corresponding code */
```

Then you can use it in the following way:

```cos
val field = j.path { field };
val field = j.path { self.field };
val first = j.path { self.field(0) };
val selfValue = j.path { self("self") };
```

```cpp
auto field = j.apply("field").val;
auto field = j.apply("field").val;
auto first = j.apply("field").val.apply(0).val;
auto selfValue = j.apply("self").val;
```

#part[ Part 2: Variables]

== Type, Val, and Var

`val` and `var` are used to declare variables. `val` is immutable, while `var` is mutable.

```cos
val EnableLogging: Type = True;
```

```cpp
constexpr auto EnableLogging = true;
```

```cos
var count = 0;
count += 1;
```

```cpp
int count = 0;
count += 1;
```

The difference between `val` and `def` is that `val` is evaluated at the point of declaration, while `def` is evaluated at the point of use:

```cos
val EnableLogging = True;
/* may fail but won't cause compile error unless you use it */
def EnableLogging = Never;
```

```cpp
/* no corresponding code */
```

`type` (type aliasing) items defines types _above_ level 0 (cannot be a runtime values):

```cos
type EnableLogging = True;
```

is equivalent to:

```cos
val EnableLogging: Type = True;
```

== Type Predicates

```cos
def IsUnsigned(T: Type) = {
  T == u8 or T == u16 or T == u32 or T == u64
}
```

They are just functions that return `True` or `False`.

== Constraint

Using `assert` anywhere in the function body will cause a compile-time error if the condition is not satisfied:

```cos
def MustUnsigned(T: Type) = {
  assert(IsUnsigned(T));
  True
}
```

```cpp
template <typename T>
struct MustUnsigned {
  static_assert(IsUnsigned<T>::value);
  using type = std::true_type;
};
```

#part[ Part 3: Constructions]

== String and Templated Literals

A string literal is wrapped either by one `"` or by _at least_ three `"`.

````cos
val string = "x";
val string = """x""";
````

template literals are strings prefixed with a "template prefix function":

````cos
val string = s"x $variable y";
val string = s"""x $variable y""";
val string = s"x ${variable}_y";
val string = s"""x ${variable}_y""";
````

The signature of `s` is:

```cos
def s(implicit FmtArgs: Type, f: String, args: FmtArgs): String;
```

A developer can define their own template prefix function:

```cos
def myFmt(implicit FmtArgs: Type, f: String, args: FmtArgs): FmtResult(FmtArgs);

val string = myFmt"x $variable y";
```

== Array, Dict, and Arguments

Arrays and Dictionaries can be constructed by `()`:

```cos
val arr = (1, 2, 3);
val dict = (key: "value");
```

```cpp
auto arr = std::array{1, 2, 3};
auto dict = std::map<std::string, std::string>{{"key", "value"}};
```

In actual, they have same shape as function calls:

```cos
val arr = Array(1, 2, 3);
val dict = Dict(key: "value");

val someCall = someFunc(1, 2, 3, key: "value");
```

As alternative syntax, you can use `->` to construct dictionaries:

```cos
val dict = (key -> "value");
val (key -> value) = dict;
```

As alternative syntax, you can use `=` to call functions:

```cos
val someCall = someFunc(1, 2, 3, key = "value");
```

== Spreading

You can spread an array, a dictionary, or an argument instance:

```cos
val arr = (1, 2, 3);
val dict = (key: "value");
val args = Args(1, 2, 3, key: "value");
val someCall = someFunc(..arr, ..dict, ..args);
```

Note: the shape of spreaded values must be determined at compile-time.

== Destruction

Putting array or dictionary on left side of `=` will destruct it:

```cos
val (a, b, c) = (1, 2, 3);
val (key = value) = (key = "value");
```

```cpp
auto [a, b, c] = std::make_tuple(1, 2, 3);
auto [key, value] = std::map<std::string, std::string>{{"key", "value"}};
```

== Pattern Matching

A value can be matched by _cases_:

```cos
val hasValue = mayT match {
  case None => false
  case Some(_) => true
}
```

#part[ Part 4: Modularity]

== Visibililty

Conceptually, an item declaration only exhibit the way of using the item, and an item definition contains implementation code.

Conceptually, a declaration or a definition will be placed in either a generated public C++ header (`*.h` api files), a generated private C++ header (`*.h` private files), or a generated C++ source file (`*.cc` files).

four visibililty:
- (default): the declaration *tends* to be generated in `.cc` files, but can be lifted to `*.h` private files as well.
- private: the declaration must be generated in `.cc` files.
- pub: the declaration must be generated in `.h` api files.
- `@inline pub`: both declaration and definition are generated in `.h` api files.

Note: `@inline` is not a visibililty feature, but a compiler flag.

Only explicit ```cos pub``` items can be ensured to be seen by C++ side code.

```cos
pub def foreignCallable();
```

== Import

```cos
import (value, parse) from T;
```

is equivalent to:

```cos
val (value, parse) = import(T);
```

```cpp
using value = T::value;
using parse = T::parse;
```

== Import C++ Header

`T` can be either a scope or a string above the level 0. For example:

```cos
val jsonPath = "nlohmann/json.hpp";
import jsonPath;
```

is equivalent to:

```cos
import "nlohmann/json.hpp"
```

```cpp
#include "nlohmann/json.hpp"
```

== Typeless C++ Dependencies

You can use C++ code without specifying types, though this will make some features unavailable:

```cos
import "nlohmann/json.hpp" as nlohmann;

def NlohmannJsonValue = nlohmann.json.value;
def NlohmannJsonTag: std.c.enum(u8) = nlohmann.json.value_t;

val json: NlohmannJsonValue = nlohmann.json.parse("{ \"key\": \"value\" }");
```

```cpp
#include "nlohmann/json.hpp"

using NlohmannJsonValue = nlohmann::json;
using NlohmannJsonTag =  nlohmann::json::value_t;

auto json = nlohmann::json::parse("{ \"key\": \"value\" }");
```

For example, you cannot do pattern matching on "auto values", because that will cause unsafe typed program.

== Typing C++ Dependencies

C/C++ Types and Headers are usually awkful, so we don't provide a reliable way to automatically extract types from them. Instead, you can specify types manually:

```cos
trait NlomannJson {
  type value: Type;
  type value_t = std.c.enum(u8);
}
import "nlohmann/json.hpp" as nlohmann: NlomannJson;
val (value_t) = nlohmann;
assert(value_t == std.c.enum(u8));
```

== Import Other Cosmo Modules

```cos
import std.json;
```

```cpp
#include <cosmo/json/index.hpp> // generated
```

is equivalent to:

```cos
import "@std/json" as json;
```

```cpp
#include <cosmo/std/json/index.hpp> // generated
```

To import some specific items inside project

```cos
import self.a.b.c;
```

```cpp
#include <myOrg/project/a/b/c/index.hpp> // generated
```

is equivalent to:

```cos
import (a: (b: c)) from "@myOrg/project";
```

```cpp
#include <myOrg/project/a/b/c/index.hpp> // generated
```

== Export again

```cos
pub import (value, parse) from T;
```

== FFI Naming Convention

For an item `a.b.c` in `@org/project`.

```cpp
// import @org/project
namespace org {
namespace project {
// import a.b.c
namespace a::b {
namespace details {
item c; // generated code will be here
}
// @external(cpp)
// def name(implicit T, val: T)
template<T>
auto c(T val, Extras extrasIfAny) {
  details(extratsIfAny.ctx, val);
}

}}}
```

#part[ Part 5: Classes]

== Normal Classes

Normal classes can have val/var/def items as fields:

```cos
class Nat {
  val data = u64
  var count = 0
  def to_int(self) = data
}
```

```cpp
class Nat {
  uint64_t data;
  int count;
  uint64_t to_int() { return data; }
};
```

def items without `self` are static methods:

```cos
class Nat { ..
  def from_int(n: u64) = Nat(n)
}
```

```cpp
class Nat { ..
  static Nat from_int(uint64_t n) { return Nat(n); }
};
```

== Enum Classes

Enum classes are constructed by _cases_:

```cos
class Nat {
  case Zero
  case Succ(Nat)
}
```

```cpp
class Nat {
  std::variant<Zero, Succ> data;
};
```

It has same shape as pattern matching:

```cos
val hasSucc = nat match {
  case Zero => false
  case Succ(Nat) => true
}
```

Compared with ```cos Option(T).map```:

```cos
val hasValue = mayT.map {
  case None => false
  case Some(_) => true
}
```

Nat-Add Example:

```cos
def add(A: Nat, B: Nat): Nat = A match {
  case Zero => B
  case Succ(B) => Succ(Add(A, B))
}
```

```cpp
Nat add(Nat A, Nat B) {
  switch (A.data.index()) {
    case 0: return B;
    case 1: return Nat::Succ(add(std::get<1>(A.data)._0, B));
  }
}
```

Common methods can be specified in the default branch:

```cos
class Nat { ..
  case _ => {
    def to_int(self) = self match {
      case Zero => 0
      case Succ(n) => 1 + n.to_int()
    }
  }
}
```

== Traits

Traits are classes containing unimplemented methods, while you can provide default impls:

```cos
trait Unsigned(T: Type) {
  assert(IsUnsigned(T.value));
  def asUint64(self): u64 = staticCast(u64, self.value);
}
```

```cpp
struct UnsignedConcept {
  virtual uint64_t asUint64() = 0;
  virtual ~UnsignedConcept() = default;
};

template <typename T>
struct UnsignedModel: public UnsignedConcept {
  T& self;
  UnsignedModel(T& self) : self(self) { static_assert(IsUnsigned<T::ValueT>::value); }
  static_assert(IsUnsigned<T>::value);
  uint64_t asUint64() override {
    return static_cast<uint64_t>(self.value);
  }
};
```

== Implementations

You can implement a trait for a class:

```cos
impl Unsigned(u8) for Nat {
  def asUint64(self) = self.to_int()
}
```

```cpp
struct NatUnsignedModel: public UnsignedModel<Nat> {
  NatUnsignedModel(Nat& self) : UnsignedModel<Nat>(self) {}
  uint64_t asUint64() override {
    return self.to_int();
  }
};
```

== Trait Disambiguation

If a class implements multiple traits with the same method name, you can disambiguate them by specifying the trait:

```cos
(nat as Unsigned(u8)).asUint64();
```

```cpp
NatUnsignedModel(nat).asUint64();
```

== Trait Mixin

There are two traits having a same method, however, you want to have a function receiving an object implemented both trait. You can mix a new trait by renaming the conflict method:

```cos
trait FromBoth(X: Type, Y: Type) {
  import (*, from: fromOutput) from From(X, Self);
  import (*, from: fromError) from From(Y, Self);
}

def erase(items: X or Y, eraser: FromBoth(X, Y)) = {
  items.map {
    case X => eraser.fromOutput(X)
    case Y => eraser.fromError(Y)
  }
}
```

#part[ Part 6: Control Flow]

== Runtime If

`if` expression:

```cos
if (enableLogging) { println("Logging enabled") }
```

== Compile-Time If

if a `if` expression's condition is a boolean type expression at level 1, it will be evaluated at compile-time:

```cos
type EnableLogging = True;
if (EnableLogging) { println("Logging enabled") }
```

```cpp
if constexpr (EnableLogging) { println("Logging enabled"); }
```

```cos
val BoundInt = if (IsUnsigned(T)) { u64 } else { i64 }
```

```cpp
using BoundInt = std::conditional_t<IsUnsigned<T>::value, u64, i64>;
```

== Loopers

`for` expression:

```cos
for (i in 0..10) { println(i) }
```

```cpp
for (auto i = 0; i < 10; i++) { println(i); }
```

is equivalent to:

```cos
val i = 0;
loop {
  if (i >= 10) { break }
  println(i);
  i += 1;
}
```

```cpp
auto i = 0;
for (;;) {
  if (i >= 10) { break; }
  println(i);
  i += 1;
}
```

You can also uses `break` and `continue` in `for` and `loop`.

== For comprehension (Yielding)

```cos
def range(from: u64, to: u64) = for (i in from..to) yield i;
```

Then you can use it in the following way:

```cos
for (i in range(0, 10)) { println(i) }
```

is equivalent to:

```cos
range(0, 10).foreach { i => println(i) }
```

The yielded function is a function (C++ class) implementing `IntoIter` trait.

```cos
trait Iter[T] {
  def next(self): Option[T]
}
trait IntoIter[T] {
  def into_iter(self): Iter[T]
}
```

== Return

`return` is used to return a value from a function:

```cos
def ten(): u64 = return 10;
```

The body of for comprehension shares the "return scope" with the outer function:

```cos
def ten = for (i in 1..100000) {
  return i;
}
```

This is not very clear. You can compared with the map function with that in rust:

```cos
def hasValue(v: Option[u64]): bool = {
  v.map { return true }
  false
}
```

is *not* equivalent to:

```rust
fn has_value(v: Option<u64>) -> bool {
  v.map(|_| return true); // warning: ignored value
  false
}
```

== Try

```cos
def parsed(src: str): Result = {
  json.Value.parse(src)?;
  Ok(())
}
```

== Try Trait

```cos
trait Try(implicit Residual: Type, implicit Output: Type) {
  import (from: from_output) from From(Output, Self);
  def branch(self): ControlFlow(Residual, Error);
}
```

Result is a type implementing `Try` trait:

```cos
class Result(O: Type, E: Type)  {
  case Ok(O)
  case Err(E)
}
```

== Catch

```cos
def parseJsonFile(fileName: String): Result = {
  json.Value.parse(throw readFile(fileName))
}
```

is equivalent to

```cos
def parseJsonFile(implicit ctx: HandleCtx(PromiseHandler), fileName: String): Result = {
  json.Value.parse(ctx.handle(implicit PromiseHandler, readFile(fileName)))
}
```

```cpp
Result<Json,String> parseJsonFile(ctx: HandleCtx, fileName: String) {
  json::Value::parse(ctx.handle(readFile(fileName)))
}
```

Note: ```cos implicit ctx``` must be visible by `parseJsonFile` syntactically, like the implicit variables in scala. For example:

```cos
def main() {
  std.handle(PromiseHandler, (promise) => {
    val condVar = Signal();
    var result;
    std.cpp.async(std.cpp.launch.async) {
      result = executePromise(promise)
      condVar.signal();
    }
    condVar.wait();
    return result;
  });
  parseJsonFile(filePath); // ok!
}

parseJsonFile(filePath); // error! promise handler is not registed in the lexical context!

parseJsonFile(implicit ctx, filePath); // ok! you pass it explicitly...
```

== Catch Trait

handlers are traits implemented apply function:

```cos
trait PromiseHandler {
  def apply(implicit T: Type, promise: Promise(T)): T;
}
```

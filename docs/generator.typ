= Generator Design Notes

== Status

This file records design notes for Cosmo generators. It is not yet a normative
specification. It is self-contained, but it uses the general effect vocabulary
from `effect.typ`.

The central rule is:

```text
`yield[Y]` is the concrete resumable protocol effect.
```

When a computation has type:

```cos
R with yield[Y]
```

it may produce zero or more `Y` payloads and eventually complete with an `R`.
This is the source-level protocol that gives the compiler a clear reason to
lower the computation as a stackless state machine.

== Function Types

Functions and lambdas keep the unified shape:

```cos
A => R with E
```

A generator-capable function is not a different declaration kind. It is an
ordinary function whose effect row contains `yield[Y]`:

```cos
def range(n: i32): Unit with yield[i32] = {
  var i: i32 = 0
  while (i < n) {
    yield(i)
    i = i + 1
  }
}
```

Its function type is:

```cos
i32 => Unit with yield[i32]
```

The same rule applies to lambdas:

```cos
val emit = (value: i32): Unit => {
  yield(value)
}
```

The inferred lambda type is:

```cos
i32 => Unit with yield[i32]
```

== The Yield Operation

`yield(value)` is an effect operation:

```cos
def yield[Y](value: Y): Unit with yield[Y]
```

This signature is descriptive. The compiler may treat `yield` as a built-in
operation, but it should type-check like an operation that adds `yield[Y]` to the
current computation.

Within a `R with yield[Y]` computation, each `yield(value)` suspends the current
activation and presents `value` to the caller. Resuming the activation continues
after that `yield`.

== Generator Step Protocol

The observable protocol is:

```cos
class GenStep[Y, R] {
  case Yield(Y)
  case Ready(R)
}
```

A generator activation can be viewed as:

```cos
trait Generator[Y, R] {
  def resume(&mut self): GenStep[Y, R]
}
```

This trait is a protocol sketch, not a committed standard-library spelling. The
important behavior is:

```text
resume -> Yield(Y)
resume -> Yield(Y)
...
resume -> Ready(R)
```

After `Ready(R)`, the activation is complete. Resuming a completed activation is
an error unless a later standard library explicitly defines a different
post-completion behavior.

== Pure Computation Widening

A computation of type `T` can be widened to:

```cos
T with yield[Y]
```

The widened computation yields zero values and completes with the original `T`.

Example:

```cos
def answer(): i32 with yield[String] = {
  42
}
```

The behavior is:

```text
resume #1 -> Ready(42)
```

This rule lets a programmer force stackless generator lowering through a type
annotation without changing the body. The yield payload type still matters: it
defines the protocol that would be used if the computation did suspend.

== Inference and Annotation

If a body contains `yield(value)`, the compiler infers `yield[type(value)]`:

```cos
def one(): Unit = {
  yield(1)
}
```

Inferred type:

```cos
() => Unit with yield[i32]
```

If the signature already contains `yield[Y]`, the body must satisfy that
contract:

```cos
def ok(): Unit with yield[i32] = {
  yield(1)
}
```

This is invalid:

```cos
def invalid(): Unit with yield[i32] = {
  yield("text")
}
```

The initial design should reject multiple unrelated yield payload types in one
computation unless the programmer explicitly wraps them in a common variant:

```cos
class Event {
  case Number(i32)
  case Text(String)
}

def mixed(): Unit with yield[Event] = {
  yield(Event::Number(1))
  yield(Event::Text("done"))
}
```

== Lowering

A computation with `yield[Y]` is lowered as a stackless state machine by default.
The generated activation stores:

- the current state label;
- function parameters;
- locals that remain live across a yield point;
- pending child activations or residual operation state, if needed.

Example source:

```cos
def two(): Unit with yield[i32] = {
  yield(1)
  yield(2)
}
```

Conceptual state machine:

```text
state 0:
  state = 1
  return Yield(1)

state 1:
  state = 2
  return Yield(2)

state 2:
  state = done
  return Ready(())
```

The state machine is an implementation strategy for the `yield[Y]` protocol. It
does not change the source function type.

== Calls and Delegation

This design does not require a special generator call syntax. The important type
is still:

```cos
A => R with yield[Y]
```

When such a computation is represented as a first-class activation, it follows
the `Generator[Y, R]` protocol described above. A later surface design may
choose whether ordinary calls to `yield` computations automatically produce
generator activations, or whether an explicit activation form is needed.

If ordinary calls to `yield` computations produce activations by default, a
generator that wants to forward another generator's yields needs explicit
delegation:

```cos
def outer(): Unit with yield[i32] = {
  yield(0)
  yield from range(10)
  yield(11)
}
```

The exact spelling of `yield from` is not committed here. The requirement is
that delegation must repeatedly resume the child activation, forward each
`Yield(Y)`, and continue when the child returns `Ready(R)`.

== Boxed Activations

A generator activation may be stored behind a box or runtime handle:

```cos
Box[Generator[Y, R]]
```

Boxing is representation, not source semantics. A concrete stackless activation
can often remain unboxed when it does not escape. A boxed activation is required
when the value must be type-erased, stored heterogeneously, or outlive the stack
frame that created it.

== Safety Across Yield

Stackless lowering moves live values across `yield` points into an activation
frame. Borrow and lifetime rules must reject values that would create invalid
self-references or references that outlive their owners.

The first implementation should conservatively reject borrowed locals that
remain live across a yield point unless the type system proves they are safe.

Example shape to reject initially:

```cos
def bad(): Unit with yield[Unit] = {
  var value: i32 = 1
  val ref = &mut value
  yield(())
  use(ref)
}
```

The same safety rule applies even if a runtime later provides a fiber-backed
implementation of the generator protocol.

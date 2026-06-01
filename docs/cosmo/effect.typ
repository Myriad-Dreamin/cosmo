#import "mod.typ": *

#show: book-page.with(title: "Effect Design Notes")

= Effect Design Notes

== Status

This file records design notes for Cosmo computation effects. It is not yet a
normative specification. The current goal is to keep suspension and handler
semantics explicit without forcing one runtime representation into source-level
function types.

The companion notes are:

- `generator.typ`: the concrete `yield[Y]` protocol and stackless state-machine
  lowering.
- `async.typ`: the abstract `async` effect and the optional lowering of async
  suspension through `yield[Pending]`.

== Core Model

All functions and lambdas have one type shape:

```cos
A => R with E
```

`A` is the parameter type or parameter tuple, `R` is the value produced when the
computation completes, and `E` is an effect row. A function without effects has
the shorter shape:

```cos
A => R
```

The `with` clause describes computation behavior. It is not part of the ordinary
value type `R`, and it should not be used to smuggle runtime representation
choices into the user-facing return type.

Examples:

```cos
i32 => i32
Path => String with async
usize => Unit with yield[i32]
Request => Response with [async, throw[HttpError]]
```

`async` and `yield[Y]` are intentionally different effects:

- `async` is an abstract suspension effect. It says that a computation may
  suspend, but it does not say whether that suspension is implemented by a
  stackless frame, a fiber, a boxed runtime handle, a VM continuation, or another
  representation.
- `yield[Y]` is a concrete resumable protocol effect. It says that a computation
  may produce zero or more `Y` payloads before completing with `R`. A computation
  with `yield[Y]` has a defined generator protocol and is the source-facing way
  to request stackless state-machine lowering.

== Computation and Representation

Effects describe computation semantics. Representations describe implementation
strategy. The source type:

```cos
R with async
```

does not mean:

```cos
Generator[Pending, R]
Task[R]
Future[R]
Box[Fiber]
```

A backend or runtime may use any of those representations when they preserve the
observable `async` semantics. A public type should not need to change when the
runtime switches between stackless and stackful execution.

The source type:

```cos
R with yield[Y]
```

does commit to a generator-style protocol. See `generator.typ` for the complete
`Yield(Y)` / `Ready(R)` model. In this design, `yield[Y]` is the explicit marker
that a computation can be lowered as a stackless resumable activation.

== Effect Inference

Effects may be inferred from a function or lambda body:

```cos
def emit_one(value: i32): Unit = {
  yield(value)
}
```

The inferred type is:

```cos
i32 => Unit with yield[i32]
```

Likewise, a body that calls `reschedule()` or `await` will infer `async`:

```cos
def later(): i32 = {
  reschedule()
  42
}
```

The inferred type is:

```cos
() => i32 with async
```

For public APIs, an implementation may later require explicit effect
annotations so that adding `yield`, `async`, or another effect does not silently
change a package boundary.

== Explicit Effect Annotations

An explicit `with` clause is a public computation contract. The implementation's
inferred effects must fit inside the annotated effects.

This is valid:

```cos
def empty(): Unit with yield[i32] = {
  ()
}
```

The body never yields, but the function is explicitly exposed as a computation
that may use the `yield[i32]` protocol. It can be represented as a generator
that immediately completes with `Ready(())`.

This is invalid:

```cos
def invalid(): Unit with yield[i32] = {
  yield("text")
}
```

The body produces `yield[String]`, which does not satisfy the annotated
`yield[i32]` contract.

== Effect Widening

A pure computation of type `T` may be widened to:

```cos
T with yield[Y]
```

This widening creates a computation that yields no values and completes
immediately with the original `T`.

Example:

```cos
def answer(): i32 with yield[Pending] = {
  42
}
```

The body computes an ordinary `i32`, but the annotated result type requests the
`yield[Pending]` protocol. The widened activation has this behavior:

```text
resume #1 -> Ready(42)
```

This rule is the explicit mechanism for forcing a value-producing computation
into a resumable state-machine form without adding an ad hoc generator call
syntax.

== Async and Yield Together

A computation may carry both effects:

```cos
R with [async, yield[Pending]]
```

This means two things:

- It has abstract async suspension semantics.
- It also exposes a concrete yield protocol whose payload is `Pending`.

An async handler may implement async suspension by translating each suspension
point into `yield(Pending)`. Under that handler, a computation of type
`R with [async, yield[Pending]]` can be lowered as a stackless state machine.

The `yield` annotation does not magically make every async operation stackless.
All async operations in the body must be expressible through the selected
`yield[Pending]` protocol, or the compiler must reject the lowering. See
`async.typ` for the async-specific rules.

== Residual Effects

A computation can have residual effects in addition to `yield` or `async`:

```cos
R with [yield[Y], throw[E]]
R with [async, throw[E]]
R with [async, yield[Pending], throw[E]]
```

Residual effects are not handled by the presence of `yield` alone. A generator
or async runtime must either preserve them in its resume/poll operation or
handle them before exposing the activation. The exact spelling of effect rows
and residual propagation remains open.

== Escape and Storage

Computations are direct-style by default. A first-class activation is created
only when a computation is represented as a generator, async handle, fiber, or
boxed runtime object.

Escaping activations need ownership and lifetime rules. In particular:

- Stackless activations store live locals in an explicit frame.
- Fiber-backed activations store a runtime-owned stack or stack segment.
- Detached async work must not capture stack-only handlers or borrowed locals
  unless the type system proves that the capture can outlive the task.

These rules are representation-independent safety constraints. A stackful fiber
does not make a detached borrowed capture safe by itself.

== Open Questions

The following questions remain open:

- What is the final surface spelling for row-polymorphic effect variables such
  as `..E`?
- Which effects are reifiable across detached async task boundaries?
- How should escaping callbacks be marked in public function signatures?
- Should public declarations require explicit effect annotations?
- What is the exact ABI between residual effects and generator `resume` or async
  `poll` operations?

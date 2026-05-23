= Async Design Notes

== Status

This file records design notes for Cosmo async computations. It is not yet a
normative specification. It is self-contained, but it uses the effect vocabulary
from `effect.typ` and the concrete yield protocol from `generator.typ`.

The central rule is:

```text
`async` is an abstract suspension effect.
```

It describes that a computation may suspend, but it does not commit the function
to a generator return type, a stackless state machine, a boxed future, or a
fiber-backed runtime handle.

== Async Function Types

Functions and lambdas keep the unified shape:

```cos
A => R with E
```

An async-capable function is an ordinary function whose effect row contains
`async`:

```cos
def fetch(path: Path): String with async = {
  reschedule()
  Fs.read_to_string(path)
}
```

Its function type is:

```cos
Path => String with async
```

This means that the computation eventually produces a `String`, but it may
suspend before doing so.

== Async Is Not Generator

`R with async` is not the same type as:

```cos
R with yield[Pending]
```

The async effect is abstract. A backend or runtime may implement it with:

- a stackless state machine;
- a fiber stack;
- a boxed runtime continuation;
- a VM frame;
- a platform async primitive;
- another representation that preserves the async semantics.

The source type should remain stable when the runtime representation changes.

== Pending and Ready Model

It is useful to understand one possible async protocol as:

```cos
class Poll[R] {
  case Pending
  case Ready(R)
}
```

In that model an async activation may produce `Pending` zero or more times and
then produce `Ready(R)` exactly once:

```text
poll -> Pending
poll -> Pending
poll -> Ready(value)
```

This model explains stackless async lowering, but it is not required by the
plain `async` effect. It becomes source-visible only when async is paired with a
concrete yield protocol such as:

```cos
R with [async, yield[Pending]]
```

== Reschedule

`reschedule()` is an async operation:

```cos
def reschedule(): Unit with async
```

It requests that the current async computation suspend and be run again later.
In a stackless async implementation that uses `yield[Pending]`, it behaves like:

```text
schedule current activation for later
yield Pending
```

The scheduling step is essential. Returning `Pending` without arranging a future
resume or wakeup can permanently lose the activation.

In a fiber-backed implementation, `reschedule()` may instead enqueue the current
fiber and switch back to the scheduler. Both implementations preserve the same
source-level `async` effect.

== Return

Returning from an async computation completes it with its result:

```cos
def answer(): i32 with async = {
  42
}
```

In a pending/ready protocol this is:

```text
Ready(42)
```

An async computation completes once. Polling or resuming a completed activation
is an error unless a later runtime specification defines a different behavior.

== Await

`await` consumes an async computation and produces its completed value in the
current async computation.

Conceptual rule:

```text
await child:
  if child is ready with value:
    continue with value
  if child is pending:
    suspend the current computation
```

In a stackless implementation, the current frame records the state after the
`await` and returns `Pending` when the child is pending. In a fiber-backed
implementation, the current fiber yields to the scheduler until the child is
ready.

`await` does not catch residual effects such as `throw[E]`; those effects remain
part of the surrounding computation unless a handler deals with them.

== Async Through Yield

An async handler may implement async suspension through a generator protocol:

```cos
R with [async, yield[Pending]]
```

Under this handler:

- `reschedule()` schedules the current activation and yields `Pending`.
- `await child` yields `Pending` when `child` is pending and continues when
  `child` is ready.
- `return value` completes with `Ready(value)`.

Example:

```cos
def later(): i32 with [async, yield[Pending]] = {
  reschedule()
  42
}
```

Conceptual state machine:

```text
state 0:
  schedule current activation for later
  state = 1
  return Yield(Pending)

state 1:
  state = done
  return Ready(42)
```

This is the explicit way to request stackless async lowering. The presence of
`async` alone does not force this representation.

== Lowering Requirements

The annotation:

```cos
R with [async, yield[Pending]]
```

requires that all async suspension in the body be expressible through the chosen
`yield[Pending]` protocol. If an async operation can only be implemented by a
fiber or another runtime mechanism that cannot be translated to the yield
protocol, the stackless lowering must fail with a diagnostic.

In other words, `yield[Pending]` is a concrete lowering contract, not a magic
conversion for arbitrary async runtimes.

== Stackless and Stackful Implementations

For:

```cos
R with async
```

a runtime may choose a stackless or stackful representation.

A stackless implementation stores the current continuation as explicit state:

```text
frame {
  state
  params
  locals live across suspension
  child async state
}
```

A stackful implementation stores the current continuation in a runtime-owned
stack or stack segment:

```text
fiber {
  stack
  entry function
  scheduler state
}
```

If a fiber-backed async activation is first-class, it must be represented by an
owned runtime handle, commonly a boxed object or arena handle. The public type
should still be the async activation protocol, not `Box[Fiber]`.

For:

```cos
R with [async, yield[Pending]]
```

the selected implementation must expose the yield protocol. The default native
lowering should be stackless state-machine lowering when the body satisfies the
lowering requirements.

== Boxing

Boxing is an implementation and storage concern:

- A concrete stackless async frame may stay unboxed when it does not escape.
- A stackless activation may be boxed for type erasure or long-lived storage.
- A first-class fiber activation needs stable runtime ownership, usually a box,
  reference-counted object, or runtime handle.

The source effect `async` should not force the user-facing return type to name
the chosen storage strategy.

== Safety Across Async Suspension

Any value live across an async suspension point is part of the continuation.
Stackless implementations store it in a frame; stackful implementations keep it
on a fiber stack. The safety rule is representation-independent:

```text
Values that remain live across suspension must be suspend-safe.
```

The first implementation should conservatively reject borrowed locals that
remain live across `await` or `reschedule()` unless the type system proves the
borrow cannot outlive its owner and cannot create an invalid self-reference.

Detached async work has stricter requirements: it must not capture stack-only
handlers or borrowed locals unless the captured values are owned, lifetime-safe,
and runtime-safe for the detached execution context.

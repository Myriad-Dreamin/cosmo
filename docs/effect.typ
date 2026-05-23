= Effect Design Notes

== Status

This file records design notes for Cosmo effects. It is not yet a normative
specification. The current goal is to keep the effect model explicit and avoid
compiler magic or new surface syntax where a regular type-system rule can carry
the same meaning.

Code snippets in this document are design sketches. A snippet that mentions
future concepts such as `FnOnce`, `Owned`, `Send`, `Static`, or
`ReifiableAcrossTask` is not committing to those exact names or syntax.

Current working assumptions:

- `with` marks computation effects in type expressions.
- Computations are second-class by default.
- A computation containing `async` can be reified as a first-class future-like
  value.
- Other effects are not first-class by default unless they are explicitly boxed
  or handled into an ordinary value.

The important distinction is between a computation that runs in the current
lexical continuation and a value that can be stored, returned, scheduled, or
moved to another thread.

== Computation Types

The preferred spelling for an effectful computation is:

```cos
T with async
T with throw[E]
T with [async, throw[E]]
```

`T with async` is a type expression. It should not be spelled as `T async` or
`async T`, because those forms look like type application and can become
unstable if `T` later accepts type-level parameters.

An alias can name a computation type:

```cos
def Future[T] = T with async
```

This means `Future[T]` is a source-level name for `T with async`, not necessarily
the same thing as a concrete runtime object type. A backend may lower the default
async representation to a concrete `Future<T>`, but the language-level type is
still the computation type.

== First-Class Async

An async computation may be reified as a future-like value:

```cos
task: T => R with async

val promises: Array[R with async] = ts.map(task)
val results: Array[R] = await Future.all(promises)
```

`results` has the value type `Array[R]`. The ambient computation containing the
`await` still has the `async` effect.

With a residual exception effect:

```cos
task: T => R with [async, throw[E]]

val promises: Array[R with [async, throw[E]]] = ts.map(task)
val results: Array[R] = await Future.all(promises)
```

`results` is still `Array[R]`. The uncaught `throw[E]` belongs to the surrounding
computation, not to the bound value:

```cos
def gather[T, R, E](ts: Array[T], task: T => R with [async, throw[E]]):
  Array[R] with [async, throw[E]] = {
  val promises = ts.map(task)
  val results = await Future.all(promises)
  results
}
```

The intended row-polymorphic shape for `Future.all` is:

```cos
def Future.all[A, E](xs: Array[A with [async, ..E]]):
  Array[A] with [async, ..E]
```

`await` removes the `async` layer from the awaited computation and propagates the
remaining row effects into the current computation. It does not catch exceptions.

== Second-Class Computations

Second-class computations are useful because they do not freely escape their
handler scope. This gives effect handlers, borrowed resources, regions, and
structured concurrency a clear lifetime.

The working rule is:

```text
plain computation: runs in the current lexical continuation
boxed/future computation: can be stored, returned, or scheduled
```

This lets ordinary higher-order functions use effectful callbacks without
requiring every callback to become a heap-allocated or detached value.

The cost is that the language must say when a function parameter may escape.
This is still unresolved.

== Row-Polymorphic Callbacks

The desired simple signature for `map` is:

```cos
def map[A, B](xs: Array[A], f: A => B): Array[B]
```

The hope is that this can be understood through row polymorphism rather than
through a new contextual-block syntax. Operationally, a call such as:

```cos
def parse_all(xs: Array[String]): Array[Int] with throw[ParseError] = {
  xs.map { x =>
    parse_int(x)?
  }
}
```

should behave like the explicit row-polymorphic form:

```cos
def map[A, B, E](xs: Array[A], f: A => B with E): Array[B] with E
```

but without requiring every simple higher-order API to expose the row variable in
surface syntax.

This is a promising direction, but only for callbacks that do not escape. If
`map` stores `f`, returns it, sends it to another thread, or schedules it after
the call returns, the callback can no longer borrow the caller's lexical effect
context.

== Escape Marking Problem

The main unresolved issue is how to mark escaping callbacks without adding
confusing new syntax or special compiler knowledge for individual functions.

Examples that need a clear escape story:

```cos
def map[A, B](xs: Array[A], f: A => B): Array[B]
```

`f` should be usable with caller effects, because `map` calls it immediately and
does not store it.

```cos
def register[A, B](f: A => B): Unit
```

If `register` stores `f`, the same signature is not enough. The type must say
that `f` escapes.

```cos
def spawn(task: () => R with E): R with [async, ..E]
```

If `spawn` is scoped, the task may borrow from the current async scope, but the
returned task handle must not escape that scope.

```cos
def spawn_detached(task: () => R with E): R with [async, ..E]
```

If `spawn_detached` detaches from the lexical scope, this signature is also not
enough. The task must be closed over owned data, must not capture stack-only
handlers, and must satisfy any thread-safety and lifetime requirements of the
runtime.

== Avoiding Detached Magic

Introducing a type such as:

```cos
Detached[() => R with E]
```

is not preferred. It looks like a normal user-defined generic type, but it would
need special compiler checks:

- it must reject captured lexical handlers that cannot outlive the scope;
- it must reject borrowed stack locals;
- it must require owned or moved captures;
- it may need `Send`, `Sync`, or runtime-specific thread-safety bounds;
- it must decide which residual effects can cross a task boundary.

Those checks are real type-system and closure-capture checks, but they should not
be hidden behind a magic library type.

A less magical shape would express `spawn_detached` in terms of ordinary closure
and trait constraints:

```cos
def spawn_detached[R, E, F](task: F): R with [async, ..E]
where
  F: FnOnce[] => R with E,
  F: Owned,
  F: Send,
  F: Static,
  E: ReifiableAcrossTask
```

The exact spelling is unresolved because Cosmo does not yet have the final
surface syntax for these bounds. The design constraint is clear: the compiler
should check a general escape/capture rule, not recognize `spawn_detached` by
name.

== Scoped Spawn vs Detached Spawn

Scoped spawn and detached spawn should have different safety rules.

Scoped spawn:

```cos
with tokio {
  val handle = spawn {
    task()
  }

  await handle
}
```

The spawned computation belongs to the current async scope. It may borrow
handlers and values that are valid for that scope, but the handle must not escape
the scope unless the task and its captures are promoted to a detached-safe form.

Detached spawn:

```cos
with tokio {
  val handle = spawn_detached {
    task()
  }
}
```

The spelling above is intentionally incomplete. The detached task may outlive
the lexical scope, so the signature must express that its closure is escaping,
owned, and runtime-safe. The open question is how to express that without adding
a new capture-mode syntax. Regardless of spelling, the detached task must not
borrow stack-only handlers or local resources. If it needs a handler, that
handler must be captured as an owned, spawn-safe value or the effect must remain
in the future and be handled at `await`.

For example, this should not be accepted without further ownership proof:

```cos
val some_use = new T

with tokio(some_use) ExceptionHandler(some_use) {
  spawn_detached {
    my_task()
  }
}
```

The detached task could outlive `some_use` and the exception handler. A safe
version must either use scoped spawn, await before the scope exits, or move owned
spawn-safe handlers into the detached task.

== Open Questions

The following questions remain open:

- Does `A => B` in a function parameter position always mean a non-escaping,
  row-polymorphic callback, or does it mean a pure callback unless an effect row
  is written explicitly?
- How should an escaping callback be marked if the design avoids new contextual
  block syntax?
- Can escape be inferred from function bodies across module boundaries, or must
  it be part of the public function signature?
- What is the minimal non-magical spelling for `FnOnce`, ownership, `Send`, and
  lifetime/static constraints?
- Which effects are `ReifiableAcrossTask`? `throw[E]` can plausibly cross an
  async boundary as a failed future, but `env[R]`, `state[S]`, regions, borrows,
  and stack-only handlers probably cannot cross by default.
- How should a scoped `JoinHandle` encode that it cannot escape its async scope?
- Should `spawn { ... }` be scoped by default and require an explicit detached
  operation for fire-and-forget work?

The strongest current preference is to keep `map`-style APIs simple through
row-polymorphic non-escaping callbacks, while making escaping and detached
execution explicit through ordinary type and closure constraints rather than
through special-purpose compiler magic.

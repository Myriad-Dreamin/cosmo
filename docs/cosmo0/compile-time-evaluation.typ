= cosmo0 Compile-Time Evaluation

== Status

This file owns compile-time macro function execution for cosmo0 macro design.
The default cosmo0 subset still rejects general macro execution until an
accepted capability admits it, but the intended execution model is full C++
compile-time execution through `cosmo-jit-sys`.

The central rule is that compile-time execution is not a JavaScript host model
and not a small interpreter for a reduced expression language. Macro functions
that need C++ capability run in a `cosmo-jit-sys` clang-repl session. That lets
providers import C++ types, instantiate templates, observe Clang's layout rules,
and execute C++ code during compilation.

== Macro Function Execution

A macro function is executed from a serialized compiler input record to a
serialized compiler output record. The exact schema can evolve, but the boundary
has this shape:

```text
MacroFunctionInput:
  provider identity
  source package identity
  macro call or macro target identity
  reflection facts selected by the compiler
  admitted attributes and defaults selected by the compiler
  Expr[Untyped] fragments when the macro kind accepts expressions
  source spans and hygiene/origin metadata
  C++ import and execution context

MacroFunctionOutput:
  generated declarations
  generated Expr[Untyped] output when the macro kind accepts expressions
  consumed attributes
  diagnostics
  generated-source summary
  native support binding metadata when needed
```

The output record is not a compiler mutation handle. Generated declarations and
generated expressions are validated, name-resolved, type-checked, and lowered by
ordinary compiler phases after macro execution.

== C++ JIT Execution

`cosmo-jit-sys` is the compile-time execution substrate for macro functions that
need C++ semantics. It owns a clang-repl session and exposes enough of Clang's
model for provider code to:

- include C++ headers;
- import C++ names and types;
- instantiate C++ templates;
- query or depend on C++ layout, alignment, padding, overload resolution, and
  ABI-visible type facts;
- compile provider snippets and support wrappers;
- execute C++ code during compilation.

This is the semantic reason to use `cosmo-jit-sys` instead of a JavaScript host
JIT. A JS object, typed-array mirror, or handwritten layout table cannot
faithfully model C++ object semantics. It can get struct padding, alignment,
bit-fields, reference lifetime, overload resolution, template instantiation,
exception behavior, or ABI wrapper details wrong. If a macro provider observes
that approximation, users see confusing differences between compile-time
behavior and the C++ code that the compiler later emits or links.

For C++ facts, the source of truth is Clang through `cosmo-jit-sys`, not a JS
reimplementation of C++ type layout or execution.

== JIT Protocol

The compiler-facing protocol remains structured even though the execution engine
has full C++ capability. Provider code may execute C++ inside the JIT session,
while the compiler accepts only the serialized macro function output record from
the session boundary.

The JIT session accepts explicit inputs:

```text
CosmoJitRequest:
  provider identity
  serialized macro function input
  C++ imports and headers
  include and library search context
  provider source or generated C++ snippet
  expected exported provider entry points
  target triple and C++ standard
  LLVM/Clang toolchain identity
```

The JIT session returns structured outputs:

```text
CosmoJitResult:
  diagnostics
  serialized macro function output
  imported C++ type facts requested by the provider
  support binding metadata when generated code needs native support
  generated artifact summary
```

The JIT may execute code, allocate objects, call provider helpers, and inspect
C++ types inside the session. It must not return raw compiler mutation handles.

== Purity Contract

Macro functions are specified as pure computations over the input supplied by
cosmo0 and the declared JIT execution context. For the same provider identity,
source package, macro call or target, compiler-selected input facts, expression
fragments, C++ imports, provider source, target settings, and toolchain identity,
repeated evaluation must produce the same generated output, diagnostics,
generated-source summary, and native support binding metadata.

The compiler may cache, discard, rerun, parallelize, or compare macro function
evaluations. `cosmo-jit-sys` can execute ordinary C++ provider code, but if a
provider uses hidden mutable state or ambient effects to produce different macro
output for the same cosmo0 input, package behavior is undefined.

== Target Runtime Separation

Compile-time evaluation is real C++ execution in the provider host, but it is
not execution of the target package binary. It must be possible to run macro
functions before the target backend emits the package executable.

A provider may use clang-repl to execute provider code and imported C++ support
code. It may not treat that as permission to patch typed modules, lowering IR,
backend state, or compiler global state outside the serialized macro function
output protocol.

== Examples

Accepted C++ JIT execution shape:

```text
macro function execution:
  start a cosmo-jit-sys clang-repl session
  import <vector>, <optional>, or project headers
  instantiate and inspect C++ types with Clang layout rules
  execute provider C++ code
  return serialized macro function output
```

Rejected host approximation shape:

```text
macro function execution:
  mirror C++ structs as JavaScript objects
  guess padding and alignment in JS
  run macro logic against the guessed layout
```

The rejected shape makes compile-time behavior diverge from Clang's C++ layout
and ABI model.

Provider behavior with undefined macro semantics:

```cos
@command(version = read_file("VERSION"))
@command(seed = random())
@command(now = current_time())
```

```text
var counter = 0

macro provider(input):
  counter = counter + 1
  return generatedName("helper_" + counter)
```

These examples are not undefined because C++ JIT is incapable of file, random,
time, or mutable-state access. They are undefined when those effects change the
macro output for the same cosmo0 input.

== Review Rules

Compile-time evaluation proposals must preserve these rules:

- macro functions that need C++ type facts or C++ code execution use
  `cosmo-jit-sys`;
- C++ type layout, padding, alignment, templates, and overload rules come from
  Clang, not from a JavaScript host approximation;
- macro function output is pure with respect to cosmo0-provided input and the
  declared JIT execution context;
- JIT execution does not directly mutate compiler typed modules, lowering IR,
  backend state, or global compiler state;
- generated code still enters ordinary validation and type checking.

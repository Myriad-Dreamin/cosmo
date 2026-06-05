= cosmo0 Compile-Time Evaluation

== Status

This file owns compile-time macro function execution for cosmo0 macro design.
The default cosmo0 subset still rejects general macro execution until an
accepted capability admits it. The intended C++ execution model is no longer an
interpreter-style JIT. Macro functions that need C++ capability run through a
structured compile-time execution adapter that compiles explicit provider entry
functions against a cached Clang context.

The central rule is that compile-time execution is not a JavaScript host model,
not a small interpreter for a reduced expression language, and not a
clang-interpreter session. The execution substrate must use Clang as the source
of C++ semantics while avoiding interpreter APIs whose behavior can diverge
from normal Clang compilation.

== Why Not clangInterpreter

The first JIT design used a clang-repl / clangInterpreter-style execution
session. That path is rejected for the macro execution boundary. Interpreter
execution is a different execution mode from ordinary Clang compilation and can
miscompile or observe behavior that the final C++ backend would not reproduce.
Even when it is useful for interactive exploration, it is too risky as the
semantic substrate for compile-time macro output.

The accepted direction is:

- compile provider entry functions as ordinary Clang translation units or
  modules;
- reuse heavy parse state through PCH, precompiled headers, module caches, or an
  equivalent precompiled context;
- load and invoke the compiled entry through a stable adapter boundary;
- accept only serialized macro function output back into the compiler.

== Macro Function Execution

A macro function is executed from a serialized compiler input record to a
serialized compiler output record. The exact schema can evolve, but the boundary
has this shape:

```text
MacroFunctionInput:
  provider identity
  source package identity
  selected macro payload identity
  reflection facts selected by the compiler
  admitted attributes and defaults selected by the compiler
  selected Expr[Untyped] macro payload when the macro kind accepts expressions
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

== C++ Function Compilation Execution

cosmo0 and cosmoc each provide their own eval mode for macro functions and
other compile-time execution requests that need C++ semantics. They are two
independent compilers: cosmo0 eval must not depend on cosmoc eval, and cosmoc
eval must not depend on cosmo0 implementation internals. The shared part is the
documented eval contract: request identity, selected input facts, payload
serialization, resource bounds, purity keys, PCH/precompiled context cache
semantics, provider-entry compilation, artifact invocation, and structured
results.

The eval mode is not a separate `cosmo-cte-sys` native support package. Each
compiler owns its own execution layer that builds and executes provider entry
functions and exposes enough of Clang's model for provider code to:

- include C++ headers;
- import C++ names and types;
- instantiate C++ templates;
- query or depend on C++ layout, alignment, padding, overload resolution, and
  ABI-visible type facts;
- compile provider snippets and support wrappers as ordinary Clang code;
- execute one or more exported provider entry functions during compilation.

The expensive part of C++ compile-time execution is parsing and semantic setup,
especially for heavy headers. The execution substrate should amortize that cost
with precompiled inputs:

```text
PrecompiledContextKey:
  C++ standard
  target triple
  compiler version
  include paths
  headers/imports
  compile options
  support library identities
```

A cached precompiled context may be implemented with PCH, Clang modules, a
module cache, or another Clang-owned precompiled representation. Provider entry
functions are then compiled against that context as small translation units.

== Eval Protocol

The compiler-facing protocol remains structured even though eval mode has full
C++ capability. Provider code may execute native C++ inside the driver process,
an out-of-process helper, or a loaded artifact, while the compiler accepts only
the serialized macro function output record from the boundary.

The eval layer accepts explicit inputs:

```text
CosmoEvalRequest:
  provider identity
  serialized macro function input
  C++ imports and headers
  include and library search context
  provider source or generated C++ entry function snippet
  expected exported provider entry points
  target triple and C++ standard
  compile options and resource limits
  precompiled context key
  LLVM/Clang toolchain identity
```

The eval layer returns structured outputs:

```text
CosmoEvalResult:
  diagnostics
  serialized macro function output
  imported C++ type facts requested by the provider
  support binding metadata when generated code needs native support
  generated artifact summary
  precompiled context cache summary
```

The compiler's eval module may compile code, load a dynamic artifact, call
provider helpers, and inspect C++ types through Clang-owned facts. It must not
return raw compiler mutation handles.

== Single-Function Compile Path

The first accepted execution path should prove one small provider entry at a
time:

```text
serialized MacroFunctionInput
  -> generate provider_entry.cpp
  -> compile provider_entry.cpp against cached PCH/precompiled context
  -> load/call exported provider entry
  -> return serialized MacroFunctionOutput
```

The provider entry should be small and deterministic. Shared heavy support code
belongs in the precompiled context or a separately built support library, not in
every generated function snippet.

== Purity Contract

Macro functions are specified as pure computations over the input supplied by
cosmo0 and the declared C++ execution context. For the same provider identity,
source package, selected macro payload, compiler-selected input facts,
selected Expr[Untyped] macro payload, C++ imports, provider source, target
settings, compile options, precompiled context key, and toolchain identity,
repeated evaluation must produce the same generated output, diagnostics,
generated-source summary, and native support binding metadata.

The compiler may cache, discard, rerun, parallelize, or compare macro function
evaluations. The native execution adapter can execute ordinary C++ provider
code, but if a provider uses hidden mutable state or ambient effects to produce
different macro output for the same cosmo0 input, package behavior is
undefined.

== Target Runtime Separation

Compile-time evaluation is real C++ execution in the provider host, but it is
not execution of the target package binary. It must be possible to run macro
functions before the target backend emits the package executable.

A provider may use the compile-time execution adapter to execute provider code
and imported C++ support code. It may not treat that as permission to patch
typed modules, lowering IR, backend state, or compiler global state outside the
serialized macro function output protocol.

== Examples

Accepted C++ compile-time execution shape:

```text
macro function execution:
  build or reuse a PCH/precompiled context for <vector>, <optional>, or project headers
  compile a small provider entry function against that context with Clang
  load and execute the compiled entry
  return serialized macro function output
```

Rejected clang interpreter shape:

```text
macro function execution:
  start a clang-repl or clangInterpreter session
  incrementally interpret provider snippets
  trust interpreter behavior as macro semantics
```

Rejected host approximation shape:

```text
macro function execution:
  mirror C++ structs as JavaScript objects
  guess padding and alignment in JS
  run macro logic against the guessed layout
```

The rejected shapes make compile-time behavior diverge from ordinary Clang C++
compilation and ABI rules.

Provider behavior with undefined macro semantics:

```cos
@command(version = read_file("VERSION"))
@command(seed = random())
@command(now = current_time())
```

```text
var counter = 0

@macro def provider(input: Expr[Untyped]): Expr[Untyped] = {
  counter = counter + 1
  return generatedName("helper_" + counter)
}
```

These examples are not undefined because native C++ execution is incapable of
file, random, time, or mutable-state access. They are undefined when those
effects change the macro output for the same cosmo0 input.

== Review Rules

Compile-time evaluation proposals must preserve these rules:

- macro functions that need C++ type facts or C++ code execution use a
  Clang-backed compile-time execution adapter, not clangInterpreter;
- C++ type layout, padding, alignment, templates, and overload rules come from
  ordinary Clang compilation facts, not from a JavaScript host approximation;
- provider entry functions are compiled against a declared C++ execution
  context, preferably accelerated by PCH or an equivalent precompiled context;
- macro function output is pure with respect to cosmo0-provided input and the
  declared C++ execution context;
- native compile-time execution does not directly mutate compiler typed modules,
  lowering IR, backend state, or global compiler state;
- generated code still enters ordinary validation and type checking.

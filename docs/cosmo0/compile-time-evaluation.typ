= cosmo0 Compile-Time Evaluation

== Status

This file owns the compile-time evaluation boundary for cosmo0 macro design.
The default cosmo0 subset still rejects general compile-time execution until an
accepted capability admits it. Future macro and const-evaluation work must use
the boundary described here rather than approximating C++ execution in a
JavaScript host model.

`cosmo-jit-sys` is the compile-time execution engine for provider code that
needs full C++ capability. It is backed by clang-repl so providers can import C++
types, instantiate templates, observe Clang's C++ layout rules, and execute C++
code during compilation.

Compile-time evaluation is split into two services:

- `ConstEval`: lowers admitted attribute/default syntax to finite `ConstValue`
  data or a deterministic diagnostic.
- `ProviderEval`: executes macro providers through `cosmo-jit-sys` when the
  provider needs full C++ compile-time execution, and returns `MacroOutput`.

Neither service is a JavaScript emulation of C++ semantics. `ProviderEval` may
execute C++ code, but generated Cosmo declarations and expressions still return
to the ordinary compiler pipeline before they affect the package.

== ConstEval

`ConstEval` handles declarative data that providers may inspect. The first value
model is finite and serializable:

```text
ConstValue:
  Bool
  String
  Integer text/value
  TypeRef
  PathRef
  Array
  Record
  Tag
```

Attribute expressions remain separate from values. `AttrExpr` records the
restricted source shape of attribute arguments; `ConstValue` records the
admitted evaluated result. Providers may receive either the structured
attribute syntax, the evaluated value, or both, depending on the provider
contract.

Defaults are treated conservatively. If a field or parameter has a default, the
macro boundary may expose it only when it can be admitted as a deterministic
compile-time constant. Otherwise the compiler reports a compile-time diagnostic
instead of asking the provider to interpret ordinary source code.

== ProviderEval

`ProviderEval` evaluates a provider through an explicit protocol. The protocol is
still the compiler boundary, even when the implementation runs provider code with
native C++ JIT execution:

```text
MacroInput:
  provider identity
  reflection metadata
  admitted attributes
  admitted ConstValue data
  Expr[Untyped] fragments when the macro kind accepts expressions
  source spans
  C++ import and execution context

MacroOutput:
  generated declarations
  generated Expr[Untyped] output when the macro kind accepts expressions
  consumed attributes
  diagnostics
  optional generated-source summary
```

Generated output is validated before it enters later compiler phases. A
provider cannot directly patch typed modules, lowering IR, backend output, or
global compiler state.

== Purity Contract

Macro functions are specified as pure computations over the input supplied by
cosmo0. For the same provider identity, source package, macro call, reflection
metadata, admitted values, expression fragments, C++ imports, and JIT execution
context, repeated evaluation must produce the same generated output, consumed
attributes, diagnostics, and summaries.

The compiler may cache, discard, rerun, parallelize, or compare provider
evaluations. `cosmo-jit-sys` can execute ordinary C++ provider code, but if a
provider uses hidden mutable state or ambient effects to produce different macro
output for the same cosmo0 input, package behavior is undefined.

== C++ JIT Execution

`cosmo-jit-sys` is not a convenience helper around a separate interpreter. It is
the execution substrate for compile-time provider evaluation that requires C++
semantics. The service owns a clang-repl session and exposes enough of Clang's
model for provider code to:

- include C++ headers;
- import C++ names and types;
- instantiate C++ templates;
- query or depend on C++ layout, alignment, padding, overload resolution, and
  ABI-visible type facts;
- compile provider snippets and support wrappers;
- execute C++ code during compilation.

This matters because a JavaScript host JIT cannot faithfully model C++ object
semantics. A JS object or typed-array mirror can easily get struct padding,
alignment, bit-fields, reference lifetime, overload resolution, template
instantiation, exception behavior, or ABI wrapper details wrong. If macro
providers observe that approximation, users see confusing differences between
compile-time behavior and the C++ code that the compiler later emits or links.

For that reason, provider execution that needs C++ facts must go through Clang,
not through a JS reimplementation of C++ type layout or execution.

== JIT Protocol

The compiler-facing protocol remains structured even though the execution engine
has full C++ capability. The provider may execute C++ code inside the JIT
session, while the compiler only accepts structured macro output from the
session boundary.

The JIT session accepts explicit inputs:

```text
CosmoJitRequest:
  provider identity
  macro input serialization
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
  MacroOutput serialization
  imported C++ type facts requested by the provider
  support binding metadata when generated code needs native support
  generated artifact summary
```

The JIT may execute code, allocate objects, call provider helpers, and inspect
C++ types inside the session. It must not return raw compiler mutation handles.
Generated declarations and expression fragments are still ordinary macro output:
they are validated, name-resolved, type-checked, and lowered by the rest of the
compiler.

== Runtime Separation

Compile-time evaluation is real C++ execution in the provider host, but it is
not execution of the target package binary. It must be possible to run providers
before the target backend emits the package executable. A provider may use
clang-repl to execute provider code and imported C++ support code; it may not
treat that as permission to patch typed modules, lowering IR, or backend state
outside the `MacroOutput` protocol.

== Examples

Accepted compile-time data shape:

```cos
@command(name = "cosmoc", version = "0.1.0")
class Cli {
  @arg(long = "package", short = "p")
  val package: Option[Path]
}
```

The attribute arguments can be represented as `AttrExpr` and admitted
`ConstValue` data such as strings and paths.

Accepted C++ JIT execution shape:

```text
ProviderEval:
  start a cosmo-jit-sys clang-repl session
  import <vector>, <optional>, or project headers
  instantiate and inspect C++ types with Clang layout rules
  execute provider C++ code
  return MacroOutput
```

Rejected host approximation shape:

```text
ProviderEval:
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

Rejected provider behavior:

```text
var counter = 0

macro provider(input):
  counter = counter + 1
  return generatedName("helper_" + counter)
```

These examples are not rejected because C++ JIT is incapable of file, random, or
time access. They have undefined macro semantics when those effects change the
macro output for the same cosmo0 input. The counter example also produces
different output for repeated evaluation of the same input.

== Review Rules

Compile-time evaluation proposals must preserve these rules:

- compile-time values are finite, serializable, and deterministic;
- provider evaluation uses `cosmo-jit-sys` for full C++ compile-time execution
  when providers need C++ types or C++ code execution;
- C++ type layout, padding, alignment, templates, and overload rules come from
  Clang, not from a JavaScript host approximation;
- provider output is pure with respect to cosmo0-provided input and the declared
  JIT execution context;
- generated code still enters ordinary validation and type checking.

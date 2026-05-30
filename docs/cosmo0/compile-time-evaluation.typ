= cosmo0 Compile-Time Evaluation

== Status

This file owns the compile-time evaluation boundary for cosmo0 macro design.
The default cosmo0 subset still rejects general compile-time execution until an
accepted capability admits it. Future macro and const-evaluation work must use
the boundary described here rather than executing target program code or
depending on ambient host state.

Compile-time evaluation is split into two services:

- `ConstEval`: lowers admitted attribute/default syntax to finite `ConstValue`
  data or a deterministic diagnostic.
- `ProviderEval`: evaluates macro providers as pure computations from
  `MacroInput` to `MacroOutput`.

Neither service is the ordinary runtime, the target executable, the C++ backend,
or a general interpreter for user code.

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

`ProviderEval` evaluates a provider through an explicit protocol:

```text
MacroInput:
  provider identity
  reflection metadata
  admitted attributes
  admitted ConstValue data
  Expr[Untyped] fragments when the macro kind accepts expressions
  source spans
  capability set

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
metadata, admitted values, expression fragments, and capability set, repeated
evaluation must produce the same generated output, consumed attributes,
diagnostics, and summaries.

The compiler may cache, discard, rerun, parallelize, or compare provider
evaluations. If a provider depends on hidden mutable state or ambient effects
and produces different results for the same cosmo0 input, package behavior is
undefined. Implementations may diagnose obvious violations, but the semantics
do not preserve non-pure provider behavior.

== Capability Boundary

Compile-time evaluation does not grant ambient host capabilities by default.
The following are outside the boundary unless a future explicit capability
admits them:

- filesystem access to undeclared paths;
- command execution;
- environment inspection;
- network IO;
- time and randomness;
- target runtime execution;
- backend availability;
- dynamic loading or native plugin mutation of compiler state.

Self-hosted providers, once admitted, should run through a dedicated
compile-time evaluator or interpreter over macro IR with fuel, recursion, and
allocation budgets. Compiler-hosted providers must obey the same input/output
and capability contract even if the first implementation is not yet self-hosted.

== Runtime Separation

Compile-time evaluation is not target program execution. It must be possible to
evaluate admitted macros before the target backend emits an executable. Runtime
APIs remain unavailable unless explicitly admitted by the compile-time
capability set.

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

Rejected compile-time computation shapes:

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

The first rejected group depends on runtime or ambient host effects. The second
produces different output for the same cosmo0 input, violating the macro
function purity contract.

== Review Rules

Compile-time evaluation proposals must preserve these rules:

- compile-time values are finite, serializable, and deterministic;
- ordinary user functions are not interpreted by default;
- provider evaluation is pure with respect to cosmo0-provided input;
- side effects require explicit capabilities;
- budget and capability diagnostics are deterministic;
- generated code still enters ordinary validation and type checking.

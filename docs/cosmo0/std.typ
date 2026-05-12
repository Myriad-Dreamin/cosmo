= cosmo0 Standard APIs

== Status

This file owns source-facing core0 standard APIs and capability identifiers. The policy and Stage 1 placeholder are normative now. Specific API signatures are placeholders until later changes add them.

== Ownership

Standard APIs are the public surface available to cosmo0 source. A backend descriptor, lowered intrinsic, or extern binding may implement a standard API, but the public behavior belongs here and in the related type, expression, runtime, and package files.

Std-owned declarations may be extern-backed when early bootstrap cannot implement them in cosmo0 source. Such declarations remain standard APIs; the extern metadata is a trusted implementation detail owned by `runtime.typ`.

== Examples

Placeholder Stage 1 source-facing shape:

```cos
import core0.text.SourceText
import core0.result.Result

def load_source(path: Path): Result<SourceText, IoError> = {
  val text = Fs.read_to_string(path)
  SourceText.from_string(text)
}
```

Implementation detail that should not become source-facing API:

```text
descriptor String::len(%source_text) -> usize
descriptor Vec<Token>::push(%tokens, %token) -> Unit
extern cosmo0.extern.v0 "::cosmo0_runtime::println"
```

The first example is a shape for future API proposals to refine. The second example names internal descriptor-style operations that may implement standard APIs but should not be the public dependency of cosmo1 source.

== Capability Identifiers

Stable core0 capability identifiers are the source-facing availability names used by stage profiles. They are not descriptor family names.

The current Stage 1 identifiers are:

- `core0.stage`: stage validation and package-profile metadata.
- `core0.text`: string/text operations used by source, token, and diagnostic code.
- `core0.text-output`: deterministic text output through a trusted std API such as `std.io.println`.
- `core0.option-result-vec`: minimal option, result, and vector surface for early compiler data.
- `core0.path-fs`: path and source-file loading surface.
- `core0.char-class`: character classification helpers for lexing.

Later-stage identifiers include `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, and `core0.big-number`. The `cosmo1.stage1` profile does not require those later capabilities.

Capability identifiers should be named at the smallest useful boundary so Stage 1 can depend on a narrow set without inheriting later compiler features.

== Extern-Backed APIs

The initial trusted extern-backed std surface is intentionally small. `std.io.println(value: String): Unit` may lower to the C++ runtime symbol `::cosmo0_runtime::println` through `cosmo0.extern.v0`. This proves the API path for deterministic smoke output without adding filesystem or command execution to the first extern smoke.

Additional extern-backed std APIs require accepted capability text in this file plus matching runtime binding rules in `runtime.typ`. Filesystem, command execution, JSON bridges, and other host-backed facilities remain std-owned API areas; they SHALL NOT be added as descriptor families merely because their implementation needs runtime support.

== Stage 1 Capability Profile

The named Stage 1 profile is `cosmo1.stage1`. It requires `core0.stage`, `core0.text`, `core0.text-output`, `core0.option-result-vec`, `core0.path-fs`, and `core0.char-class`.

The validation plan for Stage 1 is tracked by the OpenSpec change `validate-cosmo1-stage1-through-cosmo0`. Proposals that fill API signatures under these capability names must update this file and the behavior-specific owner files they affect.

== Descriptor-Backed Transition Policy

Descriptor-backed implementation support is transitional unless a standard API explicitly exposes it. Source code should depend on standard capability identifiers, not on descriptor registry details.

Future descriptor/std proposals must name each changed `docs/cosmo0/` file in their design, tasks, or proposal text. If a change only moves implementation internals and does not alter source-facing behavior, it must say that it is implementation-only and explain why no `docs/cosmo0/` file changes.

== Placeholder API Areas

- Text and byte-oriented source access.
- Collections and fallible result types.
- Diagnostics, spans, and deterministic output sinks.
- Paths and source-file loading.
- JSON and package metadata bridges.
- Numeric literal preservation and later big-number support.
- Arena and identifier helpers.
- Command execution and build integration for later stages.

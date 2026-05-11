## Context

The previous staged-runtime direction treated most runtime growth as descriptor growth. That would make cosmo0's Scala implementation the long-term owner of high-level standard library APIs. The revised direction keeps descriptors as a small compiler escape hatch and moves ordinary runtime APIs into core0 standard interfaces, source modules, or extern-backed implementations.

This change also makes the cosmo0 model explicit. Every descriptor or std capability increment must update `docs/cosmo0/`, deliver the cosmo1 component it unlocks, and include tests that exercise the capability through that component.

## Goals / Non-Goals

**Goals:**

- Define the cosmo0 specification document set used by staged runtime work.
- Define the primitive descriptor boundary.
- Define the core0 standard capability model used by cosmo1 stages.
- Split descriptor/std growth into reviewable proposals that are roughly one thousand lines each.
- Require each proposal to deliver the unlocked cosmo1 component and tests.
- Require cosmo0 compiler bug fixes to synchronize implementation, spec, and regression tests.

**Non-Goals:**

- Implement the proposals in this change.
- Add every Stage 1 runtime implementation at once.
- Make JSON, filesystem, command execution, string builders, deterministic maps, or arbitrary-precision numbers descriptor families.
- Require cosmo0 to compile the full existing `library/std`.

## Decisions

### Cosmo0 Spec Model

The staged runtime work is governed by a cosmo0 spec set under:

```text
docs/cosmo0/
  spec.typ
  type.typ
  class.typ
  expr.typ
  control-flow.typ
  std.typ
  runtime.typ
  package.typ
  testing.typ
```

The intended ownership is:

- `spec.typ`: index, goals, subset boundary, stage model, and conformance rules.
- `type.typ`: primitive types, references, aliases, and standard type applications.
- `class.typ`: classes, fields, methods, constructors, variants, and allowed member forms.
- `expr.typ`: literals, calls, selection, assignment, pattern use, and expression typing rules.
- `control-flow.typ`: `if`, `while`, `loop`, `break`, `continue`, and restricted `for`.
- `std.typ`: core0 standard interfaces, modules, and capability identifiers.
- `runtime.typ`: primitive descriptors, extern ABI hooks, runtime binding requirements, and backend support.
- `package.typ`: package metadata, imports, source loading model, and stage validation.
- `testing.typ`: conformance tests, negative tests, deterministic output tests, and bug/spec sync policy.

Each implementation proposal must name the spec files it changes. If a proposal changes accepted cosmo0 behavior and no spec file changes, the proposal must explicitly justify why the change is implementation-only.

### Bug And Spec Sync Rule

When a cosmo0 compiler bug appears while implementing a staged capability:

- If the implementation violates the current spec, fix the implementation and add a regression test.
- If the intended behavior is missing from the spec, update the relevant `docs/cosmo0/*` file in the same PR.
- If the intended behavior changes, update the spec first, then update implementation and tests to match the revised rule.
- If the behavior is deliberately left unspecified, document that gap in the relevant spec file and avoid depending on it from cosmo1 source.

This prevents staged runtime work from accumulating undocumented compiler behavior.

### Proposal Unit Contract

Every descriptor/std proposal must be a vertical slice with these parts:

- Cosmo0 spec updates in `docs/cosmo0/`.
- Descriptor or std capability implementation.
- The cosmo1 component unlocked by the capability.
- Direct cosmo0 tests for the capability.
- Cosmo1 validation tests that use the capability through cosmo1 source.
- Negative tests for invalid usage, missing capability, or forbidden descriptor expansion.

The proposal size target is approximately one thousand changed lines. The line budget is a review heuristic, not a hard limit. If a slice grows too large, split by cosmo1 component or by standard capability boundary rather than by implementation layer.

### Descriptor Boundary

Descriptors remain only for compiler-essential primitives or intrinsics that cannot be expressed as ordinary cosmo0 source or standard interface declarations.

The expected primitive descriptor set is:

- `Unit`
- `Bool`
- basic scalar literal backing for integers, bytes, chars, and strings
- `usize` and other low-level scalar forms required by source offsets and LIR
- references and mutable references when they require compiler-owned representation
- branch condition and minimal equality/comparison intrinsics required by LIR/backend
- extern/runtime ABI hooks needed to bind standard modules to host/runtime symbols

The following are not descriptor families:

- JSON values and parsing
- paths and filesystem access
- command execution
- string builders and text output
- deterministic collections
- `BigInt` and `BigDecimal`
- diagnostics formatting

Those capabilities belong in core0 standard interfaces, modules, or source-compiled implementations. They may use extern-backed runtime bindings behind the standard API.

### Stage 1 Capability Profile

`validate-cosmo1-stage1-through-cosmo0` task 1.1 should define a Stage 1 primitive descriptor and core0 standard capability set, not a descriptor-only set.

Stage 1 primitive descriptors:

- `Unit`
- `Bool`
- `usize`
- basic integer scalar support
- `Byte`
- `Char`
- references and mutable references
- literal backing for bool, integer, char, and string literals
- branch, equality, and comparison intrinsics needed by typed control flow and LIR
- extern ABI hooks for std-backed source loading or output

Stage 1 core0 standard capabilities:

- `core0.stage`
- `core0.text`
- `core0.text-output`
- `core0.option-result-vec`
- `core0.path-fs`
- `core0.char-class`

Stage 1 should not require:

- `core0.json`
- `core0.command`
- `core0.arena-id`
- `core0.map-set`
- `core0.big-number`

### Reviewable Proposal Sequence

#### 1. `model-cosmo0-spec-docs`

Scope:

- Create the `docs/cosmo0/` spec skeleton.
- Define the cosmo0 subset boundary, stage model, and bug/spec sync rule.
- Add a lightweight docs build or presence validation if available.

Cosmo1 component unlocked:

- Stage profile skeleton for `packages/cosmo1`.

Tests:

- Documentation presence or build validation.
- OpenSpec validation that future staged capability proposals name their spec updates.

#### 2. `narrow-cosmo0-primitive-descriptors`

Scope:

- Define and enforce the primitive descriptor whitelist.
- Move ordinary runtime capability names out of the descriptor expansion path.
- Update `spec.typ`, `type.typ`, `expr.typ`, and `runtime.typ`.

Cosmo1 components unlocked:

- `source/span.cos`
- basic `lex/token.cos`

Tests:

- Primitive literal, branch, equality, and reference tests.
- Negative test for attempting to add a non-primitive runtime capability as a descriptor.
- Cosmo1 span/token smoke tests using primitive types.

#### 3. `add-cosmo0-extern-abi-hooks`

Scope:

- Add minimal extern/runtime ABI binding metadata.
- Track backend runtime symbols or support libraries independently from descriptor operation families.
- Update `runtime.typ`, `std.typ`, and `package.typ`.

Cosmo1 components unlocked:

- Runtime-backed text sink or source-loading shim used by diagnostics or source tests.

Tests:

- Extern binding lowering tests.
- Missing runtime symbol diagnostics.
- Cosmo1 smoke test that reaches an extern-backed std function.

#### 4. `add-core0-stage-capability-registry`

Scope:

- Add standard capability identifiers and stage requirement profiles.
- Validate primitive intrinsics and standard capabilities for requested cosmo0/cosmo1 stages.
- Update `spec.typ`, `std.typ`, `package.typ`, and `testing.typ`.

Cosmo1 components unlocked:

- Stage 1 package metadata/profile for `packages/cosmo1`.

Tests:

- Positive Stage 1 capability validation.
- Negative tests for missing `core0.text`, `core0.path-fs`, or required primitive intrinsics.
- Tests proving later capabilities do not block Stage 1.

#### 5. `add-core0-text-interfaces`

Scope:

- Add core0 `String`, text view or slice, character/byte access, and builder-like standard API.
- Prefer standard interface declarations or source modules; use extern/backend support only behind std APIs.
- Update `type.typ`, `std.typ`, and `runtime.typ`.

Cosmo1 components unlocked:

- `source/source.cos`
- `source/source_map.cos`
- text helper functions for lexing and diagnostics.

Tests:

- String length, slice, char access, byte access, and builder tests.
- Cosmo1 source text fixture that reads and slices source content.
- Negative tests for unavailable text capability.

#### 6. `add-core0-option-result-vec-minimal`

Scope:

- Add minimal standard API surface for `Option<T>`, `Result<T, E>`, and `Vec<T>`.
- Keep the user-facing API in std even if temporary lowering support remains special.
- Update `type.typ`, `class.typ`, and `std.typ`.

Cosmo1 components unlocked:

- `driver/diagnostic.cos`
- `lex/lexer.cos` token buffer.

Tests:

- `Vec` construction, push, get, len, and iteration-shape tests.
- `Option`/`Result` construction and match tests.
- Cosmo1 diagnostics list and token buffer tests.

#### 7. `add-core0-path-fs`

Scope:

- Add `Path`, `IoError`, and `Fs.read_to_string`.
- Optionally add `Fs.write_string` if it fits the line budget.
- Bind implementation through extern ABI or source-compiled std module.
- Update `std.typ`, `runtime.typ`, and `package.typ`.

Cosmo1 components unlocked:

- `source/source.cos` source loading by path.
- `driver/config.cos` input path handling.

Tests:

- Fixture file read through std FS.
- Missing FS capability diagnostic.
- Cosmo1 source loading validation test.

#### 8. `add-core0-text-output`

Scope:

- Add minimal text output API such as `TextWriter`, `Stdout`, `Stderr`, or equivalent module functions.
- Keep diagnostic formatting in cosmo1, not std.
- Update `std.typ` and `runtime.typ`.

Cosmo1 components unlocked:

- `driver/diagnostic.cos` rendering to output sink.

Tests:

- Output support-code or extern binding tests.
- Cosmo1 diagnostic render smoke test.
- Deterministic output test for stable diagnostic text.

#### 9. `add-core0-char-class`

Scope:

- Add lexer-oriented character classification helpers.
- Keep helpers as ordinary std functions over `Char`/`Byte`.
- Update `expr.typ` and `std.typ`.

Cosmo1 components unlocked:

- `lex/lexer.cos` identifier, number, whitespace, and punctuation scanning.

Tests:

- Character classification unit tests.
- Tokenization fixture for identifiers, numbers, whitespace, and punctuation.
- Negative tests for non-ASCII or unsupported classification behavior if intentionally out of scope.

#### 10. `validate-cosmo1-stage1-capability-profile`

Scope:

- Wire the Stage 1 package layout, allowed feature set, and capability profile.
- Complete the Stage 1 validation slice.
- Update `spec.typ`, `package.typ`, and `testing.typ`.

Cosmo1 components delivered:

- `source/source.cos`
- `source/span.cos`
- `source/source_map.cos`
- `driver/diagnostic.cos`
- `lex/token.cos`
- `lex/lexer.cos`

Tests:

- `check-package` validation for Stage 1.
- `compile-package` validation through cosmo0 C++ backend.
- Deterministic output validation.
- Negative fixtures for user generics, host `Type`, reflection, staging, closures, missing capability, and unsupported higher-order APIs.

### Later Stage Proposals

These should not block Stage 1.

- `add-core0-arena-id`: unlocks `syntax/ast`, later `types/ty`, `names/scope`, `ir/hir`, and `ir/tir`.
- `add-core0-json-bridge`: unlocks `syntax/json_loader`, `package/meta`, and `cache/scope_json`.
- `add-core0-deterministic-map-set`: unlocks `package/module_graph`, `names/symbol`, `names/scope`, and `names/resolve`.
- `add-core0-lossless-numeric-literals`: unlocks numeric tokens, parser literal AST, type-checking constants, and later evaluation.
- `add-core0-command`: unlocks `link/command`, `driver/compile`, build/run smoke tests, and later linking integration.

Each later proposal follows the same unit contract: cosmo0 spec updates, descriptor/std work, unlocked cosmo1 component, and tests.

## Migration Plan

1. Land this OpenSpec-only planning change.
2. Review the proposal sequence on GitHub and split any proposal whose scope is too broad.
3. Implement `model-cosmo0-spec-docs` first so later work has an explicit cosmo0 model to update.
4. Implement descriptor-boundary work before adding new std capabilities.
5. Grow Stage 1 std capabilities in the listed order until `validate-cosmo1-stage1-through-cosmo0` can check and compile the Stage 1 package.
6. Keep later-stage capabilities out of Stage 1 validation until a dedicated follow-up proposal requires them.

Rollback is additive: if a later proposal proves too broad or incorrectly scoped, remove that proposal from the sequence without changing the descriptor boundary decision or the already-reviewed spec model.

## Risks / Trade-offs

- Risk: std work reintroduces descriptor growth under a different name.
  Mitigation: keep `runtime.typ` explicit about primitive descriptor criteria and add negative tests for ordinary APIs entering the descriptor registry.

- Risk: spec writing falls behind implementation.
  Mitigation: proposal tasks require spec updates, and bug/spec sync is part of the review checklist.

- Risk: PRs exceed the reviewable line budget.
  Mitigation: split by cosmo1 component or standard capability boundary.

- Risk: Stage 1 accidentally depends on later capabilities.
  Mitigation: Stage 1 capability validation explicitly excludes JSON, command execution, arena IDs, map/set, and big-number support.

## Open Questions

- Should `StringBuilder` be part of `core0.text` or a separate `core0.text-builder` capability?
- Should Stage 1 require `Fs.write_string`, or should it only require `Fs.read_to_string` and leave generated output writing to a later profile?
- Should deterministic collections use `Map`/`Set` by default or explicit `OrderedMap`/`OrderedSet` names?
- Which docs command should become the canonical `docs/cosmo0` build/check target once the spec skeleton exists?

## Why

cosmo1 stages require standard runtime facilities at different times, and early stages should not be blocked by later capabilities. The runtime surface should not grow primarily by adding more Scala-side descriptors, because that would bake high-level standard library APIs into cosmo0 and duplicate the standard library boundary.

A staged runtime capability model should instead validate the standard interfaces, modules, and minimal primitive intrinsics required by the current stage. Descriptors remain only for compiler-essential primitives that cannot be expressed as ordinary cosmo0 source or standard interface declarations.

## What Changes

- Add a cosmo0 specification model under `docs/cosmo0/` so staged runtime work is specified before or alongside implementation.
- Add staged availability for core0 standard runtime capabilities.
- Replace descriptor-first runtime expansion with standard interface and module declarations for most facilities.
- Limit runtime descriptors to necessary primitive or intrinsic concepts such as `Unit`, `Bool`, low-level numeric/character scalar forms, references, literal backing, and extern/runtime ABI hooks.
- Model JSON values, parsing, paths, filesystem access, command execution, string building, deterministic collections, and arbitrary-precision numeric support as core0 standard interfaces, classes, modules, or extern-backed implementations rather than new descriptor families.
- Add validation for stage-required standard capability sets.
- Preserve lossless numeric literal text before arbitrary-precision standard implementations are available.
- Require every descriptor or standard capability proposal to deliver the unlocked cosmo1 component and tests that exercise that capability through cosmo1 source.

## Design Direction

Descriptors are a bootstrap escape hatch, not the default standard-library mechanism. A new runtime facility should start as a standard interface or module unless it needs compiler recognition for syntax, control flow, ABI representation, literal construction, or backend linkage.

The expected boundary is:

- **Primitive descriptors:** compiler-known minimal types and operations required by the language and LIR, such as booleans, unit, scalar literals, branch conditions, references, and ABI-only extern hooks.
- **Standard interfaces:** source-level API contracts for standard containers, strings, builders, JSON, paths, filesystem, commands, diagnostics-oriented text, and numeric libraries. These may be ordinary cosmo0 source where the subset can express them, or trusted core0 interface declarations where implementation requires host/runtime code.
- **Runtime implementations:** source-compiled core0 modules, generated support code, or extern C++ runtime symbols behind the standard interfaces.
- **Stage requirements:** named standard capabilities and primitive intrinsics required by a cosmo0 or cosmo1 stage. Later-stage capabilities do not block earlier-stage checking.

This keeps cosmo0's compiler surface small while still giving cosmo1 a stable standard API. It also makes it possible to replace extern-backed implementations with source-compiled standard library code over time without changing user-facing APIs.

## Spec Sync Rule

`docs/cosmo0/` is the model for accepted cosmo0 behavior. If work on a staged capability exposes a cosmo0 compiler bug, the same proposal must either fix the implementation to match the spec or update the spec when the intended behavior was underspecified or wrong. Tests must cover the resolved behavior.

## Capabilities

### New Capabilities

- `cosmo0-staged-runtime-capabilities`: Defines staged availability and validation of primitive runtime intrinsics and core0 standard capabilities used by cosmo0-compiled compiler stages.

### Modified Capabilities

None.

## Impact

- Adds or updates cosmo0 model specs in `docs/cosmo0/`.
- Narrows the descriptor registry to compiler-essential primitive or intrinsic support.
- Adds a standard capability registry for stage validation.
- Adds interface availability and runtime binding tests for available and unavailable standard capability sets.
- Requires each capability increment to include the cosmo1 component it unlocks and validation tests for that component.
- Avoids requiring every standard runtime implementation before earlier stages can be checked.
- Reduces long-term duplication between cosmo0 descriptors and core0/cosmo1 standard library APIs.

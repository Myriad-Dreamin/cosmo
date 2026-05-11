## 1. Cosmo0 Spec Model

- [ ] 1.1 Add the planned `docs/cosmo0/` spec layout covering `spec.typ`, `type.typ`, `class.typ`, `expr.typ`, `control-flow.typ`, `std.typ`, `runtime.typ`, `package.typ`, and `testing.typ`.
- [ ] 1.2 Define the bug/spec sync rule: cosmo0 compiler bugs found during staged capability work must update implementation, spec, and regression tests together.
- [ ] 1.3 Require each descriptor/std proposal to name the cosmo0 spec files it updates or justify why it is implementation-only.

## 2. Runtime Capability Model

- [ ] 2.1 Define standard runtime capability identifiers and stage requirement sets.
- [ ] 2.2 Add availability validation for the primitive intrinsics and standard capabilities required by a requested cosmo0 or cosmo1 stage.
- [ ] 2.3 Ensure unavailable later-stage capabilities do not block earlier stages that do not require them.
- [ ] 2.4 Update `validate-cosmo1-stage1-through-cosmo0` task 1.1 to refer to a primitive descriptor set and core0 standard capability set, not a descriptor-only set.

## 3. Primitive Descriptor Boundary

- [ ] 3.1 Define the minimal descriptor set that remains compiler intrinsic, including `Unit`, `Bool`, scalar literal backing, references, and required ABI hooks.
- [ ] 3.2 Document why JSON, filesystem, command execution, containers, string builders, and arbitrary-precision numeric facilities are not descriptor families.
- [ ] 3.3 Add validation that new runtime capabilities are expressed as standard interfaces or modules unless they explicitly require primitive compiler support.
- [ ] 3.4 Add numeric literal text preservation as a primitive/parser-facing facility before arbitrary-precision standard implementations are available.

## 4. Standard Runtime Interfaces

- [ ] 4.1 Add core0 standard interface declarations or source modules for text, text output, character classification, path/filesystem access, and `Option`/`Result`/`Vec` as Stage 1 capabilities.
- [ ] 4.2 Add core0 standard interface declarations or source modules for JSON value access, JSON parsing bridges, arena IDs, deterministic maps/sets, command execution, and arbitrary-precision numeric support as later-stage capabilities.
- [ ] 4.3 Ensure standard APIs can be source-compiled where possible or bound through extern runtime hooks where host/runtime code is required.
- [ ] 4.4 Keep ordinary standard APIs out of the descriptor registry.

## 5. Integration

- [ ] 5.1 Connect staged standard capability availability to source-level typing and import/name resolution.
- [ ] 5.2 Lower standard interface calls through ordinary calls, source-compiled implementations, or extern runtime bindings instead of descriptor intrinsics where possible.
- [ ] 5.3 Track backend runtime requirements as standard modules, support libraries, or extern symbols rather than descriptor operation families.
- [ ] 5.4 Keep descriptor lowering only for the minimal primitive/intrinsic boundary.

## 6. Initial Proposal Planning

- [ ] 6.1 Capture `model-cosmo0-spec-docs` as the current first slice for cosmo0 spec skeleton, bug/spec sync policy, Stage 1 profile skeleton, and docs validation.
- [ ] 6.2 Capture `narrow-cosmo0-primitive-descriptors` as the current primitive descriptor slice for the descriptor whitelist, `source/span.cos`, basic `lex/token.cos`, and primitive tests.
- [ ] 6.3 Capture `add-cosmo0-extern-abi-hooks` as the current extern runtime binding slice with an extern-backed cosmo1 smoke component and missing-symbol tests.
- [ ] 6.4 Capture `add-core0-stage-capability-registry` as the current stage profile slice for Stage 1 package metadata and missing-capability tests.
- [ ] 6.5 Capture `add-core0-text-interfaces` as the current text std slice for `source/source.cos`, `source/source_map.cos`, and text tests.
- [ ] 6.6 Capture `add-core0-option-result-vec-minimal` as the current collection/result std slice for `driver/diagnostic.cos`, lexer token buffers, and collection/result tests.
- [ ] 6.7 Capture `add-core0-path-fs` as the current path/filesystem std slice for source loading components and file-read tests.
- [ ] 6.8 Capture `add-core0-text-output` as the current text output std slice for diagnostic rendering and deterministic output tests.
- [ ] 6.9 Capture `add-core0-char-class` as the current lexer helper slice for `lex/lexer.cos` and tokenization tests.
- [ ] 6.10 Capture `validate-cosmo1-stage1-capability-profile` as the current Stage 1 validation slice for complete Stage 1 validation sources and package check/compile tests.

## 7. Tests

- [ ] 7.1 Add tests for stage-required standard capability validation success.
- [ ] 7.2 Add tests for diagnostics when a required standard interface, module, primitive intrinsic, or runtime binding is unavailable.
- [ ] 7.3 Add tests proving optional later-stage capabilities do not affect earlier-stage checking.
- [ ] 7.4 Add tests that JSON, filesystem, command, string builder, collection, and arbitrary-precision numeric APIs are surfaced through standard interfaces or modules rather than descriptor metadata.
- [ ] 7.5 Add tests that primitive descriptors remain intentionally small and are rejected as an expansion path for ordinary standard library APIs.
- [ ] 7.6 Require each proposal in the sequence to include cosmo1 component validation tests that exercise the new descriptor/std capability through cosmo1 source.

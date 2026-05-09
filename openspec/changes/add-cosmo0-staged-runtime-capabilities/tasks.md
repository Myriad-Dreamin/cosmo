## 1. Runtime Capability Model

- [ ] 1.1 Define runtime descriptor capability identifiers and stage requirement sets.
- [ ] 1.2 Add descriptor availability validation for a requested cosmo0 or cosmo1 stage.
- [ ] 1.3 Ensure unavailable later-stage descriptors do not block earlier stages that do not require them.

## 2. Runtime Descriptors

- [ ] 2.1 Add JSON value and JSON parse bridge descriptor metadata for metadata and parser-facing stages.
- [ ] 2.2 Add path and filesystem descriptor metadata for source loading and generated output writing.
- [ ] 2.3 Add command execution descriptor metadata as an optional later-stage capability.
- [ ] 2.4 Add numeric literal preservation support and stage hooks for later arbitrary-precision descriptors.

## 3. Integration

- [ ] 3.1 Connect staged runtime descriptor availability to source-level typing.
- [ ] 3.2 Connect staged runtime descriptor requirements to descriptor lowering and LIR runtime requirement tracking.
- [ ] 3.3 Connect staged runtime requirements to backend include or support-code emission.

## 4. Tests

- [ ] 4.1 Add tests for stage-required descriptor validation success.
- [ ] 4.2 Add tests for diagnostics when a required descriptor is unavailable.
- [ ] 4.3 Add tests proving optional later-stage descriptors do not affect earlier-stage checking.
- [ ] 4.4 Add tests for JSON, filesystem, command, and numeric-literal descriptor metadata surfaces.

## Context

Command execution is needed once cosmo1 starts orchestrating backend compilation, linking, or smoke test execution. It is intentionally later than Stage 1 because lexing and diagnostics should not require process execution.

## Goals / Non-Goals

**Goals:**

- Define a standard command API for constructing and running commands.
- Bind implementation through trusted extern runtime hooks.
- Add `link/command.cos` and driver smoke usage.
- Add capability validation and tests proving Stage 1 is not blocked by command support.

**Non-Goals:**

- Add a shell language.
- Allow arbitrary command execution during early validation stages.
- Make command execution a descriptor family.

## Decisions

### Later-Stage Capability

`core0.command` is not part of Stage 1. It should only appear in profiles that need build/link/run integration.

### Structured API

The command API should model executable path, arguments, environment, working directory, exit status, stdout, and stderr explicitly rather than shell strings.

### Runtime Binding Is Trusted

Command execution goes through trusted std declarations and extern runtime hooks, not arbitrary user FFI.

## Risks / Trade-offs

- Risk: command execution creates security and reproducibility concerns.
  Mitigation: keep it out of early validation and make tests deterministic with controlled commands or mocked runtime support.

- Risk: platform differences leak into cosmo1.
  Mitigation: standardize result shapes and keep platform details inside runtime bindings.

## Migration Plan

1. Update `docs/cosmo0/std.typ`, `runtime.typ`, and `package.typ`.
2. Add command std declarations and extern bindings.
3. Add `link/command.cos`.
4. Add later-stage profile validation and tests.

## Open Questions

- Should command output be captured as strings, byte buffers, or text output streams?
- Should command execution be disabled by default in tests unless explicitly enabled?

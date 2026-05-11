= cosmo0 Standard APIs

== Status

This file owns source-facing core0 standard APIs and capability identifiers. The policy and Stage 1 placeholder are normative now. Specific API signatures are placeholders until later changes add them.

== Ownership

Standard APIs are the public surface available to cosmo0 source. A backend descriptor, lowered intrinsic, or extern binding may implement a standard API, but the public behavior belongs here and in the related type, expression, runtime, and package files.

== Capability Identifiers

Placeholder for stable core0 capability identifiers such as text, collections, results, arena identifiers, paths, filesystem access, command execution, JSON bridges, numeric literal helpers, deterministic output, and other staged runtime surfaces.

Capability identifiers should be named at the smallest useful boundary so Stage 1 can depend on a narrow set without inheriting later compiler features.

== Stage 1 Capability Profile

The Stage 1 profile is a placeholder for APIs needed by source loading, spans, diagnostics, token definitions, and lexing. The profile is intentionally incomplete until staged changes add exact signatures and tests.

The validation plan for Stage 1 is tracked by the OpenSpec change `validate-cosmo1-stage1-through-cosmo0`. Proposals that fill this profile must update this file and the behavior-specific owner files they affect.

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

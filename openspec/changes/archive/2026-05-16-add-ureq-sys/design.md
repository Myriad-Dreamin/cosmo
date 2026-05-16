## Context

The repository already has a Rust support-library workspace under `crates/`
and a shared support-library ABI convention for C symbols, artifacts, and
link requirements. `ureq-sys` uses that pipeline to expose blocking HTTP
without adding an async runtime or broad web-client behavior to the first
tooling stack.

## Goals / Non-Goals

**Goals:**
- Add a `ureq-sys` Rust support library with stable C ABI exports.
- Provide request construction, headers, timeout, status, and buffered body
  access.
- Add Cosmo declarations that record the `ureq-sys` support-library
  dependency.
- Keep tests deterministic by using local HTTP fixtures and input-only error
  cases.

**Non-Goals:**
- Streaming response bodies.
- Cookie stores, proxy management, async APIs, or JSON-specific helpers.
- A general HTTP client abstraction outside the narrow system package.

## Decisions

- Use opaque request, response, and error handles over exposing Rust or ureq
  internals. This keeps ownership explicit and allows release functions for all
  Rust-owned allocations.
- Use pointer/length byte arguments for FFI strings and buffers. This avoids C
  string termination requirements and keeps the ABI compatible with arbitrary
  UTF-8 payloads.
- Treat HTTP status responses as responses, including non-2xx statuses. Only
  transport, invalid input, and body-read failures are reported through the
  error handle, so callers can inspect status and body consistently.
- Buffer body access for the first slice. Text access uses ureq's bounded
  `into_string`; bytes access requires a caller-supplied maximum byte count.

## Risks / Trade-offs

- Large response bodies can still consume memory within the configured limit.
  The first slice requires buffered access only and leaves streaming to a later
  change.
- Direct FFI declarations are low-level. Higher-level ergonomic string adapters
  can be added once the compiler's ABI adapter layer is expanded.

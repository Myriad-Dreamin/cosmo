## Context

Most parser-source operations are member-oriented: `self.offset`, `self.source`, `self.advance()`, `self.source.slice(...)`, and helper calls such as `is_space(...)`. These operations need concrete IR forms before control flow can lower the full file.

## Goals / Non-Goals

**Goals:**

- Lower field reads and writes.
- Lower method calls with explicit receiver passing.
- Lower parser-required `String` and primitive intrinsic operations.

**Non-Goals:**

- Dynamic dispatch.
- General std library lowering.
- Generic intrinsic expansion beyond parser needs.

## Decisions

- Lower resolved methods as direct IR calls with receiver values.
- Represent string operations as known intrinsic or runtime-backed IR operations.
- Require prior type checking to reject invalid member shapes.

## Risks / Trade-offs

- Risk: intrinsic operation names become backend API prematurely. Mitigation: scope intrinsic names to parser-required operations and keep C++ emission responsible for final runtime spelling.

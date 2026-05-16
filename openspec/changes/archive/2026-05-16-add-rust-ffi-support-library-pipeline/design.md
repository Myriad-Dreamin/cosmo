## Context

The current cosmo0 extern ABI model can record `support-library:*` backend requirements, but those values are only opaque strings. The repository also has no Rust workspace root, no shared support-library artifact convention, and no backend link-plan object that later Rust-backed packages can consume.

`uri-sys` and `ureq-sys` are planned consumers, but their domain APIs are outside this change. This slice establishes the shared pipeline layer they will use later.

## Goals / Non-Goals

**Goals:**

- Add repository Rust workspace scaffolding under `crates/`.
- Define a shared support-library id convention, Rust library target naming, C symbol prefixes, artifact names, and staged output paths.
- Validate `@extern("c", supportLibrary = "...")` values using the shared identifier rules.
- Have the C++ backend convert `support-library:*` requirements into a support-library link plan.
- Add a minimal Rust-backed smoke crate and focused Scala tests for the shared pipeline.

**Non-Goals:**

- Add `uri-sys`, `ureq-sys`, or any package-specific domain API.
- Invoke Cargo from the Scala backend.
- Teach the C++ backend to perform the final host linker invocation.
- Expand the direct C ABI beyond fixed positional function calls and simple shared ABI value shapes.

## Decisions

Support-library ids use lowercase kebab-case. This keeps extern metadata stable and readable in Cosmo source while mapping cleanly to Cargo package names such as `support-smoke`, `uri-sys`, and `ureq-sys`.

Rust library targets use `cosmo_<id-with-underscores>`. This avoids raw hyphens in linker symbols and gives every exported C symbol a predictable prefix, for example `cosmo_support_smoke_`.

Artifacts are staged under `target/cosmo/support-libraries/<profile>/<id>/`. Cargo output locations vary by platform and crate type; a repository staging layout gives the backend and linker a stable path contract independent from Cargo internals.

The C++ backend produces a link plan instead of invoking Cargo or the linker. This keeps backend emission deterministic and side-effect free while giving later command/link orchestration a concrete list of support-library artifact paths.

Artifact validation is pure and explicit. A build/link caller provides available artifacts and ABI versions to the link plan, which reports missing artifacts and ABI mismatches with stable diagnostics.

## Risks / Trade-offs

Missing Cargo invocation in the backend -> The backend cannot build support libraries by itself. This is intentional for this slice; later driver/link orchestration can run Cargo and stage artifacts before validating the link plan.

Static artifact is the default link target -> Dynamic linking is not selected automatically yet. The model carries artifact kind and platform so a later change can choose shared libraries without changing extern metadata.

The ABI contract is narrow -> Variable-length data and Rust-owned allocations require explicit exported release functions. This avoids accidental ownership transfer across the C ABI boundary.

## Migration Plan

1. Land the shared Rust workspace, support-library model, backend link plan, and smoke tests.
2. Later package-specific changes add `crates/<id>/` and `packages/<id>/` using the shared id and artifact rules.
3. Later driver/link changes may run Cargo, stage artifacts, validate the link plan, and append link arguments to host compiler invocations.

## Open Questions

- The first package-specific consumers will decide whether static linking remains the default for all Rust support libraries.
- A later packaging change should decide whether staged support-library artifacts are copied or symlinked from Cargo outputs.

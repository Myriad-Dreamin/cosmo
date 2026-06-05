## 1. Planner Model

- [ ] 1.1 Add a gated compile-time callable graph model with stable callable,
  call-site, and artifact ids.
- [ ] 1.2 Index validation callable headers before body or entry evaluation.
- [ ] 1.3 Resolve call heads with the prefix-first resolver and store delayed
  call obligations for facts that are not ready.
- [ ] 1.4 Produce deterministic graph diagnostics for unresolved heads and
  unsupported call shapes.

## 2. Dependency Scheduling

- [ ] 2.1 Build call dependency edges after call obligations resolve.
- [ ] 2.2 Topologically schedule non-recursive callable artifacts.
- [ ] 2.3 Detect strongly connected components and classify supported direct or
  mutual recursive callable groups.
- [ ] 2.4 Reject unsupported callable cycles with stable diagnostics.

## 3. CTE Compile Boundary

- [ ] 3.1 Extend the probe adapter shape to carry callable artifact source,
  entry call identity, argument payloads, bounds, target settings, precompiled
  context key, compile options, and toolchain identity.
- [ ] 3.2 Route accepted callable artifacts through `cosmo-cte-sys` for
  integration execution or a request-compatible test double for pure tests.
- [ ] 3.3 Enforce recursion execution bounds and report a stable failure
  diagnostic when a bound is exceeded.

## 4. Fixtures

- [ ] 4.1 Add a dependent helper fixture where a constant function call uses a
  helper fact produced by the callable graph.
- [ ] 4.2 Add a source-order-independence fixture where caller and helper appear
  in both source orders, and assert the helper is checked once per plan.
- [ ] 4.3 Add a direct recursion fixture with a bounded terminating result.
- [ ] 4.4 Add a mutual recursion fixture with a bounded terminating result.
- [ ] 4.5 Add unresolved, unsupported, unavailable-CTE, provider-entry
  compile-failed, execution-failed, and recursion-bound diagnostics fixtures.

## 5. Validation

- [ ] 5.1 Assert deterministic graph, artifact, diagnostic, and result ordering
  across repeated runs.
- [ ] 5.2 Verify the validation mode is disabled by default.
- [ ] 5.3 Run the relevant cosmo0 tests and `scripts/check-scala-style.sh` if
  Scala sources are edited.

## 1. Package Layout

- [ ] 1.1 Create the `packages/cosmo0` directory structure for main and test Scala sources.
- [ ] 1.2 Add a `cosmo0` Scala package namespace that is separate from the existing `cosmo` package implementation.
- [ ] 1.3 Add the minimal source files needed for the package to compile.

## 2. Build Integration

- [ ] 2.1 Add `packages/cosmo0` to the sbt build as an independent Scala.js project.
- [ ] 2.2 Wire any required dependency on the existing parser package without changing the full compiler entry points.
- [ ] 2.3 Ensure the existing `packages/cosmo` project and tests remain addressable through their current build targets.

## 3. Public API Skeleton

- [ ] 3.1 Add a `Cosmo0` facade exposing initial parse, check, and compile entry points.
- [ ] 3.2 Add shared result, diagnostic, source span, source file, and phase status types.
- [ ] 3.3 Make unimplemented later phases return structured pending or unsupported results rather than throwing generic exceptions.

## 4. Smoke Tests

- [ ] 4.1 Add tests proving the `cosmo0` package can be instantiated.
- [ ] 4.2 Add tests proving the facade can accept source text and return structured results.
- [ ] 4.3 Run the new package test target and the existing full compiler test target.

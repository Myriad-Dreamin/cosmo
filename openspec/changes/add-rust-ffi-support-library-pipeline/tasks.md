## 1. Rust Workspace

- [ ] 1.1 Add repository-level Rust workspace scaffolding for support libraries under `crates/`.
- [ ] 1.2 Define artifact naming and output layout for Rust-backed support libraries.

## 2. C ABI Contract

- [ ] 2.1 Define shared C ABI conventions for exported Rust functions and value shapes.
- [ ] 2.2 Define support-library identifiers used by Cosmo extern bindings.

## 3. Backend and Link Integration

- [ ] 3.1 Connect `support-library:*` requirements to build and link steps.
- [ ] 3.2 Report missing or incompatible support-library artifacts clearly.

## 4. Validation

- [ ] 4.1 Add a minimal Rust-backed support-library smoke test.
- [ ] 4.2 Add backend or linker tests proving support-library requirements are consumed.
- [ ] 4.3 Document the shared Rust support-library workflow.

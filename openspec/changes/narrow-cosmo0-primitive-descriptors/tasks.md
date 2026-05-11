## 1. Spec Updates

- [ ] 1.1 Update `docs/cosmo0/type.typ` with primitive type and scalar literal rules.
- [ ] 1.2 Update `docs/cosmo0/expr.typ` with boolean branch, equality, and comparison primitive behavior.
- [ ] 1.3 Update `docs/cosmo0/runtime.typ` with the primitive descriptor whitelist and rejection criteria.

## 2. Descriptor Boundary

- [ ] 2.1 Add an implementation-level primitive descriptor allowlist.
- [ ] 2.2 Diagnose or reject new non-primitive runtime descriptor families.
- [ ] 2.3 Preserve existing temporary descriptor paths required by already implemented standard generic lowering.

## 3. Cosmo1 Components

- [ ] 3.1 Add or extend `source/span.cos` using `usize` offsets and primitive comparisons.
- [ ] 3.2 Add or extend basic `lex/token.cos` token kind data using primitive tags and booleans.

## 4. Tests

- [ ] 4.1 Add positive tests for primitive literals, boolean branches, equality/comparison, and references.
- [ ] 4.2 Add negative tests for adding JSON, filesystem, command, or string-builder APIs as descriptors.
- [ ] 4.3 Add cosmo1 span/token smoke tests.

## Why

Opening `samples/HelloWorld/main.cos` currently produces many diagnostics in VSCode even though the sample is intended to be valid and is covered by the repository's compiler/sample paths. These diagnostics are false positives, so they make the language server look unreliable and obscure real errors.

## What Changes

- Replace the current diagnostics suppression approach with test-driven fixes in the compiler-facing analysis path.
- Add unit regressions that prove the observed false positives before changing behavior, including `cosmo1.type.missing-return` and `cosmo1.type.unsupported-expr`.
- Fix Cosmo1 checker behavior so supported valid source constructs do not emit checker diagnostics.
- Align editor diagnostics with compiler-compatible package, module, std, and prelude context instead of filtering diagnostic code lists.
- Keep real parser, type mismatch, missing import, and package graph diagnostics publishable and URI/package-root attributed.
- Use focused experiments and tests to decide whether `cosmo1.type.unsupported-name` and `cosmo1.type.invalid-assignment-target` are context construction bugs, checker classification bugs, or real source diagnostics.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `cosmos-diagnostics-pipeline`: Aligns editor diagnostics with compiler-compatible source analysis and preserves real source diagnostics without editor-side suppression.

## Impact

- Future implementation will touch Cosmo1 checker/type inference tests and implementation, `packages/cosmos` diagnostics tests, and possibly the VSCode host wrapper if it is constructing the wrong package/module snapshot for sample files.
- Existing suppression helpers and gap cascade logic should be removed rather than expanded.
- The change should preserve real parser diagnostics, type mismatch diagnostics, missing import/package graph diagnostics, and genuine invalid assignment diagnostics.
- This proposal is independent from active-editor diagnostic refresh; it fixes the content of diagnostics for the HelloWorld sample.

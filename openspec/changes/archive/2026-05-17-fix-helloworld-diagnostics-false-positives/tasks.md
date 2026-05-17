## 1. Test-First Reproduction

- [x] 1.1 Add or update direct diagnostics tests that reproduce the current `samples/HelloWorld/main.cos` false-positive diagnostic codes before fixing behavior.
- [x] 1.2 Add a checker-level regression proving a function whose body is inferable as `Unit` does not emit `cosmo1.type.missing-return`.
- [x] 1.3 Add a checker or diagnostics regression proving a real supported type mismatch, such as `def main(): i32 = { false }`, still emits a checker diagnostic.
- [x] 1.4 Add a diagnostics regression proving missing import or package graph failures are preserved and attributed to the relevant URI/package root.
- [x] 1.5 Add a diagnostics regression proving supported HelloWorld constructs do not emit `cosmo1.type.unsupported-expr`.

## 2. Compiler and Context Fixes

- [x] 2.1 Remove editor-side diagnostic suppression helpers, code-list filtering, and gap cascade suppression from the current implementation.
- [x] 2.2 Fix package root, module path, std/prelude, or package-surface context construction so editor analysis matches the compiler-valid path for `samples/HelloWorld/main.cos`.
- [x] 2.3 Fix Cosmo1 return inference or block typing so supported `Unit`-returning bodies do not emit `cosmo1.type.missing-return`.
- [x] 2.4 Fix compiler-facing checker support for valid HelloWorld expression constructs that currently emit `cosmo1.type.unsupported-expr`.
- [x] 2.5 Experiment with focused tests for `cosmo1.type.unsupported-name` to determine whether the root cause is missing std/prelude/package context or a real unknown-symbol diagnostic; implement the source fix only after the test identifies the case.
- [x] 2.6 Experiment with focused tests for `cosmo1.type.invalid-assignment-target` to determine whether the root cause is expression/statement classification or a real invalid LHS; implement the source fix only after the test identifies the case.

## 3. Validation

- [x] 3.1 Verify the real `samples/HelloWorld/main.cos` URI publishes `[]` diagnostics through the VSCode-launched host.
- [x] 3.2 Verify an intentionally invalid parser edit of HelloWorld still publishes parser diagnostics.
- [x] 3.3 Verify real checker errors remain publishable after the false positives are fixed.
- [x] 3.4 Run focused Cosmo1 checker tests, Cosmos diagnostics tests, and VSCode host smoke tests that cover this change.

## Why

Opening `samples/HelloWorld/main.cos` currently produces many diagnostics in VSCode even though the sample is intended to be valid and is covered by the repository's compiler/sample paths. These diagnostics are false positives, so they make the language server look unreliable and obscure real errors.

## What Changes

- Add `samples/HelloWorld/main.cos` as an editor diagnostics regression fixture.
- Align Cosmos document diagnostics with the parser/checker/package context that treats the sample as valid.
- Ensure analyzer gaps for known-valid sample constructs do not surface as source diagnostics unless the compiler-facing pipeline also rejects the source.
- Add host-level smoke coverage so the VSCode-launched server publishes an empty diagnostics array for `HelloWorld/main.cos`.

## Capabilities

### New Capabilities

None.

### Modified Capabilities

- `cosmos-diagnostics-pipeline`: Adds known-valid sample regression behavior and tightens the boundary between real diagnostics and analyzer false positives.

## Impact

- Future implementation will touch `packages/cosmos/src/lsp/diagnostics.cos`, related tests, and possibly the VSCode host wrapper if it is constructing the wrong package/module snapshot for sample files.
- The change should preserve real parser and checker diagnostics for invalid files.
- This proposal is independent from active-editor diagnostic refresh; it fixes the content of diagnostics for the HelloWorld sample.

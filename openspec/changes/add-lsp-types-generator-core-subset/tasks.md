## 1. Metamodel Input

- [ ] 1.1 Check in the `lsprotocol` metamodel input used by the generator.
- [ ] 1.2 Model references, arrays, maps, unions, and literal object shapes needed by the initial subset.
- [ ] 1.3 Keep generation deterministic and offline.

## 2. Generated Core Subset

- [ ] 2.1 Emit common data structures such as `Position`, `Range`, `Location`, `Diagnostic`, `MarkupContent`, and `Hover`.
- [ ] 2.2 Emit the initial session messages for `initialize`, `initialized`, `shutdown`, `exit`, and core text-document notifications.
- [ ] 2.3 Emit a stable package layout under `packages/lsp-types/`.

## 3. Validation

- [ ] 3.1 Add deterministic regeneration tests or snapshots.
- [ ] 3.2 Compile the generated package as part of validation.
- [ ] 3.3 Document how the generator is invoked through `cosmo -p <package> run`.

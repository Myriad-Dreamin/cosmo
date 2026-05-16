## 1. Metamodel Input

- [x] 1.1 Check in the `lsprotocol` metamodel input used by the generator.
- [x] 1.2 Model references, arrays, maps, unions, and literal object shapes needed by the initial subset.
- [x] 1.3 Keep generation deterministic and offline.

## 2. Generated Core Subset

- [x] 2.1 Emit common data structures such as `Position`, `Range`, `Location`, `Diagnostic`, `MarkupContent`, and `Hover`.
- [x] 2.2 Emit the initial session messages for `initialize`, `initialized`, `shutdown`, `exit`, and core text-document notifications.
- [x] 2.3 Emit a stable package layout under `packages/lsp-types/`.

## 3. Validation

- [x] 3.1 Add deterministic regeneration tests or snapshots.
- [x] 3.2 Compile the generated package as part of validation.
- [x] 3.3 Document how the generator is invoked through `cosmo -p <package> run`.

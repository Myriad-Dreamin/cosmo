## 1. Metamodel Input

- [x] 1.1 Download the full `lsprotocol` metamodel through `ureq-sys`.
- [x] 1.2 Keep the downloaded full metamodel ignored by git.
- [x] 1.3 Model references, arrays, maps, unions, null unions, and literal object shapes needed by the full generated surface.

## 2. Generated Full Surface

- [x] 2.1 Emit full generated type aliases, enums, structures, requests, and notifications.
- [x] 2.2 Follow `lspt`-style flavors for `Uri`, `HashMap`, `UnionN`, request, and notification modules.
- [x] 2.3 Emit a stable package layout under `packages/lsp-types/src/lsp/full/`.

## 3. Validation

- [x] 3.1 Add deterministic regeneration tests or snapshots.
- [x] 3.2 Compile the Cosmo generator package as part of validation.
- [x] 3.3 Document how the generator is invoked through `cosmo -p <package> run` and `yarn gen:lsp-types`.

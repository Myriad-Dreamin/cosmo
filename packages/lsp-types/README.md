# @cosmo/lsp-types

`packages/lsp-types` owns the Cosmo generator and checked-in full LSP type surface used by language-server work.

## Full Metamodel

The full LSP 3.17 metamodel is downloaded on demand with the repository `ureq-sys` crate and is intentionally ignored by git:

```sh
yarn fetch:lsp-types
```

That writes `metamodel/metaModel.json`. Generate the checked-in lspt-flavored full-spec type surface with:

```sh
yarn gen:lsp-types
```

Or run both steps:

```sh
yarn generate:lsp-types
```

The full output lives under `src/lsp/` and mirrors the `lspt` crate shape: `base`, `type_aliases`, `enums`, `structs`, `request`, and `notification`, with `Uri`, `HashMap`, and `UnionN` helper flavors.

The generator logic is written in Cosmo under `src/generator/`. `scripts/genLspTypesFromMetamodel.js` is only a file-writing wrapper around:

```sh
yarn cosmo -p packages/lsp-types run
```

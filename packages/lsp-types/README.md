# @cosmo/lsp-types

`packages/lsp-types` owns the checked-in LSP core subset used by early Cosmo language-server work.

The generator consumes `metamodel/lsp-core-subset.metamodel.json`, a curated offline subset derived from the official LSP 3.17 metamodel:

https://github.com/microsoft/language-server-protocol/blob/gh-pages/_specifications/lsp/3.17/metaModel/metaModel.json

Regenerate and inspect the checked-in protocol types with:

```sh
yarn cosmo -p packages/lsp-types run
```

The command writes the deterministic generated `src/lsp/core.cos` content to stdout. To refresh the checked-in output:

```sh
yarn cosmo -p packages/lsp-types run > packages/lsp-types/src/lsp/core.cos
```

Validation is covered by the Cosmo0 package tests and `yarn validate:lsp-types`. They compile the package and compare the generator stdout with `src/lsp/core.cos`.

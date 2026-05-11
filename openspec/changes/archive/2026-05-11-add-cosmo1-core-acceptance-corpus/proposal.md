## Why

cosmo0 needs a concrete acceptance target before implementation starts, otherwise the subset boundary can drift toward either toy examples or the full Cosmo compiler. A single cosmo1-style corpus file gives the cosmo0 pipeline a shared reference for the main compiler data types and expression forms it must eventually accept, without implementing cosmo1 itself.

## What Changes

- Add a cosmo1 core acceptance corpus as one Cosmo source file.
- Cover representative compiler-oriented types such as spans, diagnostics, symbols, tokens, AST nodes, arenas, typed IDs, maps, options, results, and parser state.
- Cover representative expression forms such as class fields, enum-style cases, methods, references, mutation, variant construction, match, loops, conditionals, assignments, method calls, and descriptor-backed standard generic usage.
- Treat the corpus as an acceptance fixture and design target, not as a working cosmo1 compiler implementation.

## Capabilities

### New Capabilities

- `cosmo1-core-acceptance-corpus`: Defines the representative cosmo1-shaped source forms that cosmo0 is expected to parse, elaborate, type, lower, and eventually compile.

### Modified Capabilities

None.

## Impact

- Adds test data under the cosmo0/cosmo1 validation area.
- Establishes a shared fixture for later cosmo0 elaborator, typer, LIR lowering, and backend changes.
- Does not change the existing full Cosmo compiler path.

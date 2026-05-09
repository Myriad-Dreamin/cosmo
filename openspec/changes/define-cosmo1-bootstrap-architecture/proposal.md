## Why

cosmo0 defines the small Scala-implemented core that can compile bootstrap-sensitive code, but the project still needs a concrete target for the compiler written in that subset. Defining cosmo1's file architecture, required data types, and compiler capabilities now prevents cosmo0 from growing blindly and gives bootstrap work a testable destination.

## What Changes

- Define cosmo1 as the Cosmo compiler written in the cosmo0 subset.
- Specify a proposed cosmo1 package/file layout covering driver, source management, lexer/parser, syntax, package loading, name resolution, type checking, compile-time evaluation, IR lowering, C++ code generation, linking, artifacts, and tests.
- List the standard library types cosmo1 requires from cosmo0/core0, including sealed generics such as `Vec<T>`, `Option<T>`, `Result<T, E>`, `Map<K, V>`, `Set<T>`, `Arena<T>`, `Id<T>`, `Box<T>`, and pointer/reference forms.
- List cosmo1-owned compiler data types such as spans, diagnostics, tokens, AST nodes, symbols, scopes, type representations, compile-time values, typed IR, and C++ emission structures.
- Define the compiler capabilities cosmo1 must eventually implement and separate them from the host-language capabilities cosmo0 must provide.
- Define staged bootstrap targets so implementation can proceed without requiring complete full-Cosmo compatibility at the first milestone.

## Capabilities

### New Capabilities

- `cosmo1-file-architecture`: Defines the package and source-file layout for cosmo1.
- `cosmo1-core-types`: Defines the core0 standard types and cosmo1-owned data types required to write cosmo1 in cosmo0.
- `cosmo1-compiler-capabilities`: Defines the compiler features cosmo1 must implement and the cosmo0 language/runtime features needed to express them.
- `cosmo1-bootstrap-stages`: Defines staged milestones and acceptance criteria for building cosmo1 incrementally.

### Modified Capabilities

None.

## Impact

- Adds an OpenSpec planning layer above `introduce-cosmo0-core`; this change does not implement cosmo1 and does not change existing compiler behavior.
- Future implementation is expected to add a new cosmo1 package, cosmo0-compatible source files, core0 descriptor coverage for required standard types, and smoke tests that compile cosmo1 components through cosmo0.
- The proposal intentionally keeps full Cosmo language features as data and algorithms inside cosmo1 rather than as required cosmo0 host-language features.

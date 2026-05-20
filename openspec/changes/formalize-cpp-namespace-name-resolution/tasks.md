## 1. Import Model And Parsing

- [ ] 1.1 Extend the parser/elaborator import model to represent Cosmo module imports separately from C++ namespace imports.
- [ ] 1.2 Accept `import <cpp-namespace> as <alias> from "c++/<header>"` for C++ namespace imports.
- [ ] 1.3 Reject `import "c++/<header>"` with an unsupported C++ header-only import diagnostic.
- [ ] 1.4 Add import parsing tests for valid namespace imports, merge-compatible repeated imports, incompatible aliases, and rejected header-only imports.

## 2. Name Resolution

- [ ] 2.1 Add a foreign namespace alias binding kind with canonical C++ namespace path, local alias, contributing headers, and source span data.
- [ ] 2.2 Merge repeated C++ namespace imports only when the local alias and canonical C++ namespace target match.
- [ ] 2.3 Report conflicts when a C++ namespace alias collides with a Cosmo declaration/import or maps the same alias to a different C++ namespace.
- [ ] 2.4 Resolve qualified paths by resolving the leftmost segment first and dispatching subsequent segments by binding kind.
- [ ] 2.5 Ensure C++ aliases do not make the original C++ namespace name or any nested C++ symbol implicitly visible.

## 3. Package Graph And Backend Inputs

- [ ] 3.1 Exclude C++ namespace imports from Cosmo package dependency edges.
- [ ] 3.2 Preserve contributing C++ header sources as backend include requirements or foreign binding inputs.
- [ ] 3.3 Add diagnostics or validation hooks for C++ qualified suffixes that cannot be found in the alias header set when C++ symbol validation is available.

## 4. Documentation

- [ ] 4.1 Add a name-resolution document that formalizes resolver inputs, outputs, phase order, unqualified lookup, qualified lookup, foreign namespace aliases, and diagnostics.
- [ ] 4.2 Update syntax/import documentation to state that `import "c++/<header>"` is unsupported and that C++ imports require an explicit namespace alias.
- [ ] 4.3 Document C++ namespace merge imports with the `std as cstd` from `"c++/vector"` and `"c++/string"` examples.

## 5. Fixtures And Tests

- [ ] 5.1 Add `fixtures/name-resolution` positive fixtures for lexical lookup, qualified lookup, and compatible C++ namespace alias merging.
- [ ] 5.2 Add `fixtures/name-resolution` negative fixtures for unresolved names, duplicate bindings, incompatible C++ namespace aliases, Cosmo/C++ alias conflicts, and unsupported header-only C++ imports.
- [ ] 5.3 Wire resolver tests to run the `fixtures/name-resolution` corpus and check stable diagnostic codes/spans.
- [ ] 5.4 Run the relevant parser, package graph, name-resolution, and fixture tests.

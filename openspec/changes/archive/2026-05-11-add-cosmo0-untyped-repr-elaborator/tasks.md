## 1. Untyped Representation

- [x] 1.1 Define untyped module, declaration, statement, expression, pattern, parameter, and type representation nodes.
- [x] 1.2 Add stable source span attachment to every untyped node that can produce diagnostics.
- [x] 1.3 Represent type applications, references, paths, calls, selections, blocks, variants, and match constructs without depending on full Cosmo IR.

## 2. Elaboration Pipeline

- [x] 2.1 Add an elaborator entry point from shared parser syntax into the cosmo0 untyped representation.
- [x] 2.2 Normalize parser AST forms that are syntactic sugar into explicit cosmo0 untyped nodes.
- [x] 2.3 Convert compile-time apply syntax used for canonical standard generic type application into untyped type representations only in type positions.
- [x] 2.4 Preserve enough original source context to report unsupported constructs with useful diagnostics.

## 3. Subset Boundary

- [x] 3.1 Accept non-generic classes, enum-style cases, functions, fields, locals, simple aliases, imports, blocks, calls, selections, assignments, returns, conditionals, loops, and match expressions.
- [x] 3.2 Reject user-defined generic classes, functions, traits, impls, explicit type parameters, host `Type`, type functions, reflection, staging, quotation, paste operations, closures, lambdas, and unsupported higher-order APIs.
- [x] 3.3 Reject generic type applications outside registered standard-generic syntax positions.
- [x] 3.4 Emit deterministic diagnostics for every rejected construct covered by the subset boundary.

## 4. Tests

- [x] 4.1 Add positive elaboration tests for representative accepted core declarations and expressions.
- [x] 4.2 Add negative elaboration tests for rejected full-language constructs.
- [x] 4.3 Add span-preservation tests for accepted nodes and rejection diagnostics.

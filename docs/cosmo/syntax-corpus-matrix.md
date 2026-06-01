# cosmo0 Syntax Corpus Matrix

This document makes the current cosmo0 sample and diagnostic corpus reviewable as
a syntax matrix. It is not a full grammar specification; the normative language
boundary remains in the adjacent `docs/cosmo/*.typ` files.

The matrix intentionally references corpus files instead of embedding source
programs. Concrete programs live in `samples/**/*.cos` and
`fixtures/diagnostics/*.cos`; this document stays at the review and coverage
layer so the abstraction does not drift into another copy of the implementation.

Positive source files are expected to compile through `Cosmo0().compile` or
`Cosmo0().compilePackage`. Diagnostic files use the convention documented in
`fixtures/diagnostics/README.md` and are expected to produce at least the listed
diagnostic at the listed source position.

## Corpus Index

### Positive Source Corpus

| ID | Path | Pipeline | Primary role |
| --- | --- | --- | --- |
| P01 | `samples/HelloWorld/main.cos` | source compile | Executable-shaped source and trusted runtime print calls. |
| P02 | `samples/Arithmetics/scalars.cos` | source compile | Primitive numeric and boolean scalar expressions. |
| P03 | `samples/Text/scalars.cos` | source compile | String, byte, char, and text descriptor methods. |
| P04 | `samples/Classes/tokens.cos` | source compile | Concrete classes, fields, constructors, and methods. |
| P05 | `samples/Collections/option_result_vec.cos` | source compile | Stage 1 `Option`, `Result`, and `Vec` APIs. |
| P06 | `samples/Collections/arena_map_set.cos` | source compile | `Arena`, `Id`, `Map`, and `Set` accepted API surface. |
| P07 | `samples/ControlFlow/search.cos` | source compile | `while`, `for`, `if`, `return`, local mutation. |
| P08 | `samples/References/mutability.cos` | source compile | Reference types and receiver mutability. |
| P09 | `samples/Patterns/result_match.cos` | source compile | Variant-pattern `match` with payload bindings. |
| P10 | `samples/Packages/basic` | package compile | Package metadata, modules, imports, and public functions. |

### Diagnostic Source Corpus

| ID | Path | Expected diagnostic | Primary role |
| --- | --- | --- | --- |
| D01 | `fixtures/diagnostics/parse_missing_initializer.cos` | `cosmo0.parse.failed` | Parser failure boundary. |
| D02 | `fixtures/diagnostics/type_unresolved_name.cos` | `cosmo0.type.unresolved-name` | Value lookup failure. |
| D03 | `fixtures/diagnostics/type_wrong_call_arity.cos` | `cosmo0.type.wrong-arity` | Call arity checking. |
| D04 | `fixtures/diagnostics/type_invalid_field.cos` | `cosmo0.type.invalid-field` | Field and method selection failure. |
| D05 | `fixtures/diagnostics/type_assignment_mismatch.cos` | `cosmo0.type.assignment-mismatch` | Assignment type checking. |
| D06 | `fixtures/diagnostics/type_return_mismatch.cos` | `cosmo0.type.return-mismatch` | Function return checking. |
| D07 | `fixtures/diagnostics/type_branch_mismatch.cos` | `cosmo0.type.branch-mismatch` | Branch result unification. |
| D08 | `fixtures/diagnostics/type_invalid_mutability.cos` | `cosmo0.type.invalid-mutability` | Immutable receiver mutation rejection. |
| D09 | `fixtures/diagnostics/unsupported_generic_class.cos` | `cosmo0.elaborate.unsupported.generic-class` | Unsupported user generic declaration. |
| D10 | `fixtures/diagnostics/unsupported_lambda.cos` | `cosmo0.elaborate.unsupported.lambda` | Unsupported function value syntax. |
| D11 | `fixtures/diagnostics/package_missing_import.cos` | `cosmo0.package.missing-import` | Package module graph missing import. |
| D12 | `fixtures/diagnostics/package_private_import_member.cos` | `cosmo0.package.missing-import-member` | Package visibility boundary. |
| D13 | `fixtures/diagnostics/package_duplicate_declaration.cos` | `cosmo0.package.duplicate-declaration` | Package declaration namespace collision. |
| D14 | `fixtures/diagnostics/type_unsupported_map_key.cos` | `cosmo0.type.unsupported-map-key` | Deterministic collection key restriction. |

### Workspace Corpus

| ID | Path | Validator | Primary role |
| --- | --- | --- | --- |
| W01 | `samples/cosmo.json` | `scripts/validateSampleWorkspaces.mjs` | Virtual root opened at `samples/`. |
| W02 | `samples/workspaces/virtual-root` | `scripts/validateSampleWorkspaces.mjs` | Nested virtual workspace root. |
| W03 | `samples/workspaces/package` | `scripts/validateSampleWorkspaces.mjs` | Ordinary package workspace. |
| W04 | `samples/workspaces/single-file/main.cos` | `scripts/validateSampleWorkspaces.mjs` | Package-less single-file workspace. |

## Syntax Variant Matrix

| ID | Area | Small class | Variants covered | Positive refs | Diagnostic refs | Status |
| --- | --- | --- | --- | --- | --- | --- |
| S01 | File shape | Single source file | Top-level declarations in one `.cos` file | P01-P09 | D01-D10, D14 | Covered |
| S02 | File shape | Multi-file diagnostic fixture | `/// path`, `/// diag`, `/// end path` convention in `.cos` files | - | D01-D14 | Covered |
| S03 | File shape | Package directory | `cosmo.json`, `src/*.cos`, module paths | P10 | D11-D13 | Covered |
| S04 | File shape | Workspace root | Virtual workspace and ordinary package sample layouts | W01-W04 | - | Covered by workspace validator |
| S05 | Declarations | Top-level function | `def name(params): Return = body` | P01, P02, P03, P07, P08, P09, P10 | D02, D03, D06, D07, D10, D11-D13 | Covered |
| S06 | Declarations | Top-level class | Concrete `class` with member declarations | P04, P06, P07, P08, P09 | D04, D08, D14 | Covered |
| S07 | Declarations | Top-level type alias | `type Name = Target` | P06 | - | Positive only |
| S08 | Declarations | Public visibility | `pub def` exported across modules | P10 | - | Positive only |
| S09 | Declarations | Private visibility | `private def` not importable from another module | - | D12 | Negative only |
| S10 | Declarations | User generic exclusion | User-defined type parameter list on class | - | D09 | Negative only |
| S11 | Imports | Module import | `import module` with dependency ordering | P10 | D11 | Covered |
| S12 | Imports | Import member from module | `import name from module` visibility check | - | D12 | Negative only |
| S13 | Package graph | Duplicate declaration | Same declaration name in multiple modules | - | D13 | Negative only |
| S14 | Functions | Parameter annotations | Primitive, user, standard generic, and reference parameter types | P02, P03, P07, P08, P09 | D02, D06, D07 | Covered |
| S15 | Functions | Return annotations | Explicit `Unit`, primitive, user, standard generic, and reference-compatible returns | P01-P10 | D06 | Covered |
| S16 | Functions | Block body result | Final expression provides block/function value | P02, P03, P04, P05, P06, P08, P09, P10 | D06, D07 | Covered |
| S17 | Functions | Runtime-call body | Trusted `println` calls inside ordinary functions | P01, P10 | - | Positive only |
| S18 | Variables | Local immutable binding | `val name = expr` and inferred local type | P01, P05, P06, P07, P10 | D10 | Covered |
| S19 | Variables | Local mutable binding | `var name: Type = expr` and later assignment | P07 | - | Positive only |
| S20 | Variables | Top-level mutable binding | `var name: Type = expr` at module scope | - | D05 | Negative type case only |
| S21 | Fields | Immutable field | `val field: Type` in class | P04, P07, P09 | D04 | Covered |
| S22 | Fields | Mutable field | `var field: Type` in class | P06, P08 | D08 | Covered |
| S23 | Receivers | Immutable self receiver | `def method(&self)` | P04, P06, P08 | D08 | Covered |
| S24 | Receivers | Mutable self receiver | `def method(&mut self)` | P06, P08 | - | Positive only |
| S25 | References | Immutable reference type | `&T` parameter type | P08 | - | Positive only |
| S26 | References | Mutable reference type | `&mut T` parameter type | P08 | D08 | Covered |
| S27 | References | Mutable receiver method call | Passing a mutable reference to a mutable method | P08 | D08 | Covered |
| S28 | Types | Primitive scalar types | `Unit`, `Bool`, `i32`, `usize`, `f64`, `u8`, `Char`, `String` | P01, P02, P03 | D05, D06, D07 | Covered |
| S29 | Types | User class type | Concrete class names in fields, params, returns, constructors | P04, P06, P07, P09 | D04, D14 | Covered |
| S30 | Types | Standard generic type | `Vec[T]`, `Option[T]`, `Result[T, E]`, `Arena[T]`, `Id[T]`, `Map[K, V]`, `Set[T]` | P05, P06, P07, P09 | D03, D14 | Covered |
| S31 | Types | Unsupported map/set key | User class key in deterministic collection | - | D14 | Negative only |
| S32 | Literals | Integer literals | Context-typed integer literals in arithmetic, calls, assignments | P01, P02, P04, P05, P07, P10 | D06 | Covered |
| S33 | Literals | Floating literals | Context-typed `f64` arithmetic | P02 | - | Positive only |
| S34 | Literals | Boolean literals | `true`/`false` and boolean return expectations | P02 | D05 | Covered |
| S35 | Literals | String literals | String construction, comparison, diagnostics payload text | P01, P03, P04, P05, P06, P07, P09, P10 | D07 | Covered |
| S36 | Calls | Free function call | Cross-module and same-module ordinary calls | P10 | D02 | Covered |
| S37 | Calls | Runtime function call | Trusted `println` with string and numeric arguments | P01, P10 | - | Positive only |
| S38 | Calls | User constructor call | `ClassName(args...)` from field order | P04, P06 | - | Positive only |
| S39 | Calls | Standard constructor call | `Vec[T]()` and variant constructors | P05, P07, P09 | D03 | Covered |
| S40 | Calls | Method call | Descriptor and class method selection followed by call | P03, P04, P05, P06, P08 | D04 | Covered |
| S41 | Calls | Higher-order and lambda exclusion | Function value syntax rejected before relying on call lowering | - | D10 | Negative only |
| S42 | Selection | Field selection | `receiver.field` over user classes | P04, P06, P09 | D04 | Covered |
| S43 | Selection | Method selection | `receiver.method(...)` over user and descriptor-backed types | P03, P04, P05, P06, P08 | D04 | Covered |
| S44 | Operators | Numeric arithmetic | `+`, `-`, `*`, `/`, `%` over primitive numeric types | P01, P02, P04, P08, P10 | - | Positive only |
| S45 | Operators | Numeric comparison | `<`, `>=`, and mixed comparison in boolean result | P02, P07 | - | Positive only |
| S46 | Operators | Equality | `==` over strings and numeric values | P03, P07, P10 | - | Positive only |
| S47 | Operators | Boolean connectives | `and` and unary `!` | P02, P06, P07 | D07 | Covered |
| S48 | Assignment | Local assignment | `name = expr` on mutable local | P07 | - | Positive only |
| S49 | Assignment | Field assignment | `receiver.field = expr` through mutable receiver | P06, P08 | D08 | Covered |
| S50 | Assignment | Type mismatch | Assignment value does not conform to target type | - | D05 | Negative only |
| S51 | Control flow | `if` with else | Value-producing conditional with matched branch type | P05, P10 | D07 | Covered |
| S52 | Control flow | `if` without else | Statement-style conditional | P07 | - | Positive only |
| S53 | Control flow | `while` loop | Boolean condition, mutation inside body | P07 | - | Positive only |
| S54 | Control flow | `for` loop | Iteration over `Vec[T]` | P07 | - | Positive only |
| S55 | Control flow | `return` | Early return from function body | P07 | D06 | Covered |
| S56 | Sequencing | Semicolon expression statement | Semicolon separates side-effecting calls from final expression | P01, P05, P06, P08, P10 | - | Positive only |
| S57 | Match | Standard variant pattern | `Option[T]::Some`, `Option[T]::None`, `Result[T, E]::Ok`, `Result[T, E]::Err` | P05, P09 | - | Positive only |
| S58 | Match | Payload binding | Pattern payload introduces local binding with variant field type | P05, P09 | - | Positive only |
| S59 | Match | Branch type unification | All arms produce one expression type | P05, P09 | D07 | Partially covered |
| S60 | Diagnostics | Parser diagnostic | Malformed declaration initializer | - | D01 | Covered |
| S61 | Diagnostics | Name diagnostic | Unresolved value name | - | D02 | Covered |
| S62 | Diagnostics | Arity diagnostic | Too many call arguments | - | D03 | Covered |
| S63 | Diagnostics | Selection diagnostic | Missing field or method | - | D04 | Covered |
| S64 | Diagnostics | Mutability diagnostic | Immutable receiver mutation | - | D08 | Covered |

## Review Gaps

| Gap | Current state | Suggested corpus action |
| --- | --- | --- |
| Top-level immutable `val` positive sample | Compiler accepts simple top-level values, but current positive corpus focuses on functions/classes. | Add a small source sample or fold one into an existing scalar sample if top-level values are intended as public corpus surface. |
| Positive `import name from module` sample | Member import is represented only through a private-member diagnostic. | Add a package sample with a public imported member if this syntax should be encouraged. |
| Text API negative fixture | Text methods are covered positively, but invalid text calls share general call/field diagnostics. | Add a focused diagnostic fixture only if text-specific diagnostics are introduced. |
| Variant pattern arity diagnostic fixture | Wrong variant payload arity is covered in Scala tests, not this convention corpus. | Add a `.cos` diagnostic fixture for pattern payload arity. |
| Literal pattern compile-positive sample | Literal patterns parse and type-check, but current lowering rejects literal pattern lowering. | Add only after LIR lowering supports literal patterns. |
| `break` and `continue` positive samples | Parser and lowering have support, but current corpus does not expose them. | Add a loop-control sample if these forms are part of the intended cosmo0 user surface. |
| Unsupported trait, impl, host `Type`, staging, and higher-order APIs | Some are covered in existing unit tests or legacy/stage fixtures, but not in `fixtures/diagnostics`. | Add convention fixtures when the diagnostic corpus is expanded beyond the current minimal set. |


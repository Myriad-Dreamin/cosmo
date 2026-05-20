= cosmo0 Specification

== Status

This skeleton is the review entry point for the cosmo0 language and runtime model. The subset boundary, document ownership, conformance overview, and sync policy links are normative now. Detailed behavior sections that point to later capability work are placeholders until a future OpenSpec change fills them.

== Subset Boundary

cosmo0 is the bootstrap subset used to compile staged cosmo1 compiler components and the core0 support surface. A conforming cosmo0 implementation accepts only source constructs, standard APIs, runtime hooks, and package behaviors described by this document set or by an accepted OpenSpec change that updates this document set.

The subset is intentionally smaller than full Cosmo. User-defined generic programming, trait solving, type-level evaluation, reflection, staging macros, broad parser-combinator APIs, and arbitrary full-language features are outside cosmo0 until a later spec file admits them.

Source-facing behavior is specified by the `docs/cosmo0/` files. Compiler descriptors, backend intrinsics, and extern hooks are implementation mechanisms unless `runtime.typ` says they are visible capability behavior.

== Examples

Accepted shape for a small bootstrap data type:

```cos
class Span {
  val start: usize
  val end: usize
}

class Token {
  val span: Span
  val text: String
}

def is_empty(span: Span): Bool = span.start == span.end
```

Rejected shape until later specs admit user-defined generics and function values:

```cos
def map_tokens[T](tokens: Vec<Token>, f: Token => T): Vec<T> = {
  tokens.map(f)
}
```

The accepted example is intentionally simple: it uses concrete classes, fields, primitive and standard types, and a non-generic function. The rejected example crosses multiple full-language boundaries at once.

== Document Ownership

- `spec.typ` owns the index, subset boundary, conformance overview, and cross-document policy.
- `type.typ` owns primitive types, references, aliases, and standard type application.
- `class.typ` owns class, field, method, constructor, and variant subset rules.
- `expr.typ` owns expression typing and literal, call, selection, match, and mutation rules.
- `control-flow.typ` owns accepted branch, loop, and return forms.
- `name-resolution.typ` owns source binding, qualified lookup, foreign namespace aliases, and name diagnostics.
- `std.typ` owns core0 standard interfaces and capability identifiers.
- `runtime.typ` owns primitive descriptors, extern ABI hooks, and backend runtime requirements.
- `package.typ` owns package metadata, imports, source loading, module ordering, and stage validation.
- `testing.typ` owns positive, negative, deterministic, and bug regression test policy.

== Conformance Overview

A cosmo0 implementation conforms to this skeleton when it:

- rejects source outside the documented subset before relying on lowering or backend behavior;
- reports unsupported constructs with diagnostics that identify the rejected feature area;
- keeps public source behavior stable through the owning `docs/cosmo0/` file;
- keeps generated package, module, and runtime output deterministic for repeated inputs;
- updates specs and regression tests when a bug fix changes intended behavior.

Detailed acceptance criteria for each behavior area belong in the owning file named above. Until such criteria are written, the implementation must not treat an undocumented behavior as a cosmo1 bootstrap requirement.

== Stage 1 Capability Profile

cosmo0 package validation may select a named stage capability profile. A profile records the primitive descriptor support, core0 standard capabilities, and backend extern/runtime requirements that must be available before a package is accepted for that stage.

The first profile is `cosmo1.stage1`. It covers the source, span, source-map, diagnostic, token, lexing, source-loading, character-classification, and deterministic text-output surface needed by cosmo1's first executable compiler slice. The profile is cross-referenced by the OpenSpec change `validate-cosmo1-stage1-capability-profile`.

Stage 1 capability ownership is split across:

- `std.typ` for standard API capability identifiers;
- `runtime.typ` for backend and extern support behind those APIs;
- `package.typ` for source loading and stage validation expectations;
- `testing.typ` for acceptance, regression, and sync checks.

Stage validation is requirement-based: a missing required primitive descriptor, standard capability, or backend requirement is a package-check diagnostic. Capabilities not named by the selected profile do not block validation for that stage.

== Sync Policy

Bug fixes and staged runtime proposals must follow the policy in `testing.typ`. Future descriptor or standard API proposals must name the changed `docs/cosmo0/` files, or explicitly justify why the change is implementation-only.

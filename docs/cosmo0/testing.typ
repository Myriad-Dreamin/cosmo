= cosmo0 Testing and Spec Sync

== Status

This file owns testing policy for the cosmo0 model. The bug/spec sync rule and spec impact review rule are normative now. Concrete test matrices are placeholders until later implementation changes add behavior-specific tests.

== Positive Tests

Placeholder for accepted source examples that prove documented cosmo0 behavior works through the intended pipeline.

== Examples

Positive test shape:

```cos
class Span {
  val start: usize
  val end: usize
}

def empty(span: Span): Bool = span.start == span.end
```

Negative test shape:

```cos
def id[T](value: T): T = value
```

Bug regression checklist shape:

```text
1. Add the smallest source case that reproduces the bug.
2. Assert the expected diagnostic or successful output.
3. Update the owning docs/cosmo0/*.typ file if intended behavior changed.
```

The positive example should compile once its owning language sections are fully specified. The negative example should be rejected because user-defined generic functions are outside the initial cosmo0 subset.

== Negative Tests

Placeholder for unsupported syntax, type, declaration, expression, control-flow, runtime, and package cases that must be rejected with diagnostics.

== Determinism Tests

Placeholder for package ordering, runtime support emission, generated code, and diagnostic stability checks.

== Capability Validation Tests

Stage capability validation tests should include:

- positive validation for each named profile against the default compiler capability availability;
- negative diagnostics for missing primitive descriptors, standard capabilities, and backend extern/runtime requirements;
- package-level tests for metadata-driven profile selection;
- tests proving capabilities outside a selected profile do not block that stage.

For `cosmo1.stage1`, missing `core0.text`, missing `core0.path-fs`, and missing primitive intrinsics must produce `cosmo0.stage.missing-capability` diagnostics. The same profile must validate without requiring `core0.json`, `core0.command`, `core0.arena-id`, `core0.map-set`, or `core0.big-number`.

== Bug Regression Tests

Placeholder for tests added with bug fixes. A regression test should capture the smallest source or model case that would fail if the bug returned.

== Bug/spec Sync Rule

When implementation behavior conflicts with the current cosmo0 docs, first decide whether the implementation or the docs are wrong.

If the implementation is wrong, the fix must add or update a regression test and leave the owning `docs/cosmo0/` file unchanged except for clarifying text.

If the intended behavior is missing or wrong in the docs, the same change must update the owning `docs/cosmo0/` file before cosmo1 source or staged runtime work relies on that behavior. The change must also add tests proving the documented behavior.

Undocumented implementation behavior must not become a cosmo1 bootstrap dependency just because the current compiler accepts it.

== Spec Impact Review Rule

Future descriptor/std proposals must name each changed `docs/cosmo0/` file, or explicitly justify implementation-only status. This applies to descriptor registry changes, standard API changes, extern/runtime hooks, package behavior, and Stage 1 capability updates.

Reviewers should check the proposal, design, and task list for a concrete spec impact statement before accepting staged runtime work. Acceptable statements include a list of updated files such as `docs/cosmo0/std.typ` and `docs/cosmo0/runtime.typ`, or an implementation-only justification that says no source-facing behavior changed.

== Lightweight Skeleton Validation

The lightweight validation command is `node scripts/validateCosmo0Docs.js`. It checks that required `docs/cosmo0/` files exist, that the Stage 1 and sync policy placeholders are present, and that active staged runtime proposals reference their spec impact or implementation-only status.

== Staged Runtime Proposal Guidance

A staged runtime proposal is review-ready only when its tasks show how the spec impact will be validated. That proof can be a docs-only check, a targeted compiler test, or a clear review step tied to the affected `docs/cosmo0/` file.

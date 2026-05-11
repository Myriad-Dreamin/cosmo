= cosmo0 Testing and Spec Sync

== Status

This file owns testing policy for the cosmo0 model. The bug/spec sync rule and spec impact review rule are normative now. Concrete test matrices are placeholders until later implementation changes add behavior-specific tests.

== Positive Tests

Placeholder for accepted source examples that prove documented cosmo0 behavior works through the intended pipeline.

== Negative Tests

Placeholder for unsupported syntax, type, declaration, expression, control-flow, runtime, and package cases that must be rejected with diagnostics.

== Determinism Tests

Placeholder for package ordering, runtime support emission, generated code, and diagnostic stability checks.

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

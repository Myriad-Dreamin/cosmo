# Cosmo Macro Expr / Compile-Time Evaluation Notes

This note has been split into the cosmo0 documentation set so it can be indexed
with the rest of the site and reviewed through the same ownership rules.

Current pages:

- `docs/cosmo0/macro-expr.typ` owns `Expr[T = Untyped]`, expression macro
  input/output, typed inspector access, and expression macro non-goals.
- `docs/cosmo0/compile-time-evaluation.typ` owns macro function input/output
  records, macro function purity, full C++ compile-time execution through
  `cosmo-jit-sys`, and target runtime separation.

The original mixed discussion should not be extended here. New macro expression
or compile-time evaluation changes should update the owning `docs/cosmo0/*.typ`
page and the matching OpenSpec change.

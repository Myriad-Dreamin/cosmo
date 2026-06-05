# add-cosmoc-repl-eval-mode

Add a cosmoc REPL mode that evaluates interactive input through cosmoc's own
eval module. The REPL reuses provider-entry compilation and PCH/precompiled
context caching, but it is not clang-repl and it does not depend on cosmo0 eval.

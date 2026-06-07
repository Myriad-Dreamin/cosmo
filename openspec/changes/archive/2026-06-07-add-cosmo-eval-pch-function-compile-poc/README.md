# add-cosmo-eval-pch-function-compile-poc

Define the cosmo eval contract for Clang-backed compile-time execution in both
compilers. cosmo0 and cosmoc implement independent eval modules; each module
owns its own PCH/precompiled context caching and explicit provider-entry
compilation. The change does not introduce a separate `cosmo-cte-sys` native
support package or use clang-repl / clangInterpreter as the execution substrate.

The implemented PCH executable backend is a semantic and benchmark proof, not
the accepted low-latency backend for REPL or production compile-time
evaluation. Benchmarks showed repeated provider-entry compile/link remains too
slow even without `-O2`, `-O3`, debug, or LTO flags. A follow-up backend must
preserve ordinary Clang compile semantics while avoiding per-request executable
compile/link cost.

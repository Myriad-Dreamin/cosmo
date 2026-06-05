# add-cosmo-eval-pch-function-compile-poc

Define the cosmo eval contract for Clang-backed compile-time execution in both
compilers. cosmo0 and cosmoc implement independent eval modules; each module
owns its own PCH/precompiled context caching and explicit provider-entry
compilation. The change does not introduce a separate `cosmo-cte-sys` native
support package or use clang-repl / clangInterpreter as the execution substrate.

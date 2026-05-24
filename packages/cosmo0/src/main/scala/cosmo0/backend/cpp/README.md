# cpp

This directory contains the C++ backend.

`Backend.scala` exposes `CppBackend` and `CppOutput`. It validates input with
`LirTypeChecker`, maps LIR declarations and operations to C++, records backend
requirements, and builds a `SupportLibraryLinkPlan` from those requirements.

`NlohmannJsonDependency.scala` describes the external nlohmann JSON dependency
used by the runtime bridge.

Inputs come from `lir/Lir.scala`. Link and runtime artifacts are coordinated
with `backend/support/Library.scala`.

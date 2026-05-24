# backend

Backends consume checked LIR and produce host artifacts or link metadata.

`cpp/` contains the C++ emitter and C++-specific runtime dependency metadata.
`support/` contains support-library identifiers, artifact metadata, and link
plan construction shared by backend output and package/runtime integration.

Backend code depends on `lir/` for compiler IR and on `syntax/ExternAbi.scala`
for trusted external ABI requirements. It should not depend on parser syntax or
source elaboration details.

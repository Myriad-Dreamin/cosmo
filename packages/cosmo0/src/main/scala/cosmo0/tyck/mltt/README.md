# mltt

This directory contains the Scala mirror for the experimental MLTT core checker.

`TypeChecker.scala` defines MLTT terms, contexts, declaration metadata,
metavariables, conversion, bidirectional infer/check entry points, and
diagnostics.

It is profile-gated and separate from the default cosmo0 source checker. The
ordinary compiler pipeline does not elaborate user source into MLTT core yet;
tests and profile fixtures exercise this component directly.

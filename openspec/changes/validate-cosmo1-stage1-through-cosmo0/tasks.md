## 1. Stage Definition

- [ ] 1.1 Define the cosmo1 Stage 1 descriptor set required for source loading, spans, diagnostics, tokens, lexing-oriented structures, and text output.
- [ ] 1.2 Define the Stage 1 package layout and metadata used by cosmo0 package validation.
- [ ] 1.3 Document which cosmo0 language and runtime features Stage 1 is allowed to use.

## 2. Stage 1 Source

- [ ] 2.1 Add cosmo1-style Stage 1 source files for spans, diagnostics, tokens, lexer state, and text output helpers.
- [ ] 2.2 Use only accepted cosmo0 subset constructs and implemented staged runtime descriptors.
- [ ] 2.3 Avoid user-defined generics, host `Type`, reflection, staging, closures, and unsupported higher-order APIs in Stage 1 source.

## 3. Validation Tests

- [ ] 3.1 Add check-package validation for the Stage 1 package.
- [ ] 3.2 Add compile-package validation through the cosmo0 C++ backend.
- [ ] 3.3 Add negative Stage 1 fixtures for accidental user generics, host `Type`, reflection, staging, closures, and unsupported higher-order APIs.
- [ ] 3.4 Add deterministic output validation for Stage 1 compile output.

## 4. Documentation

- [ ] 4.1 Document how to run the Stage 1 cosmo0 validation workflow.
- [ ] 4.2 Document how the Stage 1 fixture relates to the earlier cosmo1 core acceptance corpus.
- [ ] 4.3 Document the process for extending Stage 1 validation as additional descriptors and lowering support become available.

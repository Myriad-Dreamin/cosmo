## 1. Spec Updates

- [x] 1.1 Update `docs/cosmo0/std.typ` with command construction and execution APIs.
- [x] 1.2 Update `docs/cosmo0/runtime.typ` with command extern binding requirements.
- [x] 1.3 Update `docs/cosmo0/package.typ` with later-stage command capability validation.

## 2. Standard API

- [x] 2.1 Add command builder or constructor APIs.
- [x] 2.2 Add argument, environment, and working-directory APIs.
- [x] 2.3 Add execution API returning status, stdout, and stderr data.
- [x] 2.4 Keep command execution out of the descriptor registry.

## 3. Cosmo1 Components

- [x] 3.1 Add or extend `link/command.cos`.
- [x] 3.2 Add driver smoke usage for later build/link/run integration.

## 4. Tests

- [x] 4.1 Add command construction tests.
- [x] 4.2 Add controlled command execution or mocked runtime binding tests.
- [x] 4.3 Add missing `core0.command` capability diagnostics.
- [x] 4.4 Add tests proving Stage 1 validation does not require command support.

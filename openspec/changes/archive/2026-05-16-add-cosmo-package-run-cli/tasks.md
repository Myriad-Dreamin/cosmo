## 1. CLI Surface

- [x] 1.1 Parse `-p <package>` and `run` as a package-aware command form.
- [x] 1.2 Define trailing argument forwarding and working-directory behavior.
- [x] 1.3 Define runnable entrypoint resolution for package-owned tools.

## 2. Host Execution

- [x] 2.1 Load the selected package and compile the runnable target.
- [x] 2.2 Execute the compiled target through the existing host runner.
- [x] 2.3 Report missing package, missing entrypoint, and compile or run failures clearly.

## 3. Validation

- [x] 3.1 Add smoke tests for a package-owned runnable target.
- [x] 3.2 Add argument forwarding tests.
- [x] 3.3 Update contributor docs with the new invocation pattern.

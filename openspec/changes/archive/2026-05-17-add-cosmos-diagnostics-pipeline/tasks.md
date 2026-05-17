## 1. Analysis Entry

- [x] 1.1 Add `packages/cosmos` analysis entry points over parser, package, and checker stages from `packages/cosmoc`.
- [x] 1.2 Define the first refresh boundary for open-document changes.

## 2. Diagnostic Conversion

- [x] 2.1 Convert spans into LSP ranges.
- [x] 2.2 Convert severity, code, and message data into generated diagnostic types.
- [x] 2.3 Preserve deterministic ordering for repeated runs.

## 3. Publication

- [x] 3.1 Publish diagnostics on open, change, and close events.
- [x] 3.2 Clear stale diagnostics when documents or packages no longer report them.

## 4. Validation

- [x] 4.1 Add parser and checker diagnostic fixtures.
- [x] 4.2 Add package metadata or module-graph diagnostic fixtures.
- [x] 4.3 Add tests for stable diagnostic ordering and clearing behavior.

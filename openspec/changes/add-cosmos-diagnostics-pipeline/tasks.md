## 1. Analysis Entry

- [ ] 1.1 Add `packages/cosmos` analysis entry points over parser, package, and checker stages from `packages/cosmoc`.
- [ ] 1.2 Define the first refresh boundary for open-document changes.

## 2. Diagnostic Conversion

- [ ] 2.1 Convert spans into LSP ranges.
- [ ] 2.2 Convert severity, code, and message data into generated diagnostic types.
- [ ] 2.3 Preserve deterministic ordering for repeated runs.

## 3. Publication

- [ ] 3.1 Publish diagnostics on open, change, and close events.
- [ ] 3.2 Clear stale diagnostics when documents or packages no longer report them.

## 4. Validation

- [ ] 4.1 Add parser and checker diagnostic fixtures.
- [ ] 4.2 Add package metadata or module-graph diagnostic fixtures.
- [ ] 4.3 Add tests for stable diagnostic ordering and clearing behavior.

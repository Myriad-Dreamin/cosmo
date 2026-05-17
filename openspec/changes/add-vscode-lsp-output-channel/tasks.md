## 1. Output Channel

- [ ] 1.1 Create or configure a named VSCode output channel for `Cosmo Language Server`.
- [ ] 1.2 Append host probe failures, startup errors, and language-server stderr to the output channel.
- [ ] 1.3 Ensure LSP stdout remains reserved for protocol messages and is not mirrored into the output channel.

## 2. User Access

- [ ] 2.1 Add a VSCode command such as `cosmo.showLanguageServerOutput` that reveals the channel.
- [ ] 2.2 Contribute the command in `editors/vscode/package.json` with a clear title.
- [ ] 2.3 Reveal or point to the channel when activation fails before the language client starts.

## 3. Validation

- [ ] 3.1 Add extension tests or focused unit coverage that verifies the output channel is created during activation.
- [ ] 3.2 Add coverage for a probe/startup failure path that appends the failure text to the channel.
- [ ] 3.3 Manually verify stderr from the VSCode-launched host is visible while reproducing diagnostics refresh issues.

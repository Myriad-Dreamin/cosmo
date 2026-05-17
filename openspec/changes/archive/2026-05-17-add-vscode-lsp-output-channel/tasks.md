## 1. Output Channel

- [x] 1.1 Create or configure a named VSCode output channel for `Cosmo Language Server`.
- [x] 1.2 Append host probe failures, startup errors, and language-server stderr to the output channel.
- [x] 1.3 Ensure LSP stdout remains reserved for protocol messages and is not mirrored into the output channel.

## 2. Startup Metadata

- [x] 2.1 Emit a startup banner with `eprintln`/stderr from the VSCode-launched host after the host resolves its launch configuration.
- [x] 2.2 Include the git revision used to build or run the host.
- [x] 2.3 Include relevant version values, such as the Cosmo server/build version and the VSCode extension package version when available.
- [x] 2.4 Include resolved non-secret configuration needed to debug launch behavior, such as host path/command, workspace or server root, transport, and config source.
- [x] 2.5 Keep stdout reserved for LSP JSON-RPC protocol output; do not print the startup banner to stdout.

## 3. User Access

- [x] 3.1 Add a VSCode command such as `cosmo.showLanguageServerOutput` that reveals the channel.
- [x] 3.2 Contribute the command in `editors/vscode/package.json` with a clear title.
- [x] 3.3 Reveal or point to the channel when activation fails before the language client starts.

## 4. Validation

- [x] 4.1 Add extension tests or focused unit coverage that verifies the output channel is created during activation.
- [x] 4.2 Add coverage for a probe/startup failure path that appends the failure text to the channel.
- [x] 4.3 Add automated coverage or a focused smoke path proving startup metadata reaches stderr/output-channel capture.
- [ ] 4.4 Manually verify stderr from the VSCode-launched host is visible while reproducing diagnostics refresh issues.

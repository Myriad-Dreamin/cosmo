## 1. Workspace Asset

- [ ] 1.1 Add a checked-in VSCode `.code-workspace` sample that opens the existing sample package root and highlights `samples/HelloWorld/main.cos` as the primary smoke file.
- [ ] 1.2 Configure the workspace to avoid generated directories and repository build outputs.
- [ ] 1.3 Keep language activation based on ordinary `.cos` files, without requiring custom commands before the workspace opens.

## 2. Validation

- [ ] 2.1 Add a repository check that parses the workspace JSON and verifies every referenced folder/file exists.
- [ ] 2.2 Add a VSCode extension smoke note or test fixture that uses this workspace as the manual diagnostics/hover target.

## 3. Documentation

- [ ] 3.1 Update contributor or extension documentation with the exact workspace file to open from VSCode.
- [ ] 3.2 Mention the expected first-screen experience: `HelloWorld/main.cos` is available as the starting file and the Cosmo extension activates for `.cos`.

## 1. Project Structure Samples

- [x] 1.1 Add `samples/cosmo.json` as a virtual workspace root that lets users open `samples/` directly.
- [x] 1.2 Add `samples/workspaces/virtual-root` with a virtual root `cosmo.json` and ordinary package members.
- [x] 1.3 Add `samples/workspaces/package` with an ordinary package `cosmo.json`.
- [x] 1.4 Add `samples/workspaces/single-file` with no `cosmo.json`, using only standard library/prelude capabilities.
- [x] 1.5 Remove the `.code-workspace` sample direction from this change.

## 2. Validation

- [x] 2.1 Add a repository check that parses the sample `cosmo.json` files and verifies expected project-shape invariants.
- [x] 2.2 Verify every virtual workspace member path exists and points to an ordinary package manifest.
- [x] 2.3 Verify every ordinary package sample has sources under `src/`.
- [x] 2.4 Verify the single-file sample has no owning ordinary package manifest and is not listed as a workspace member.

## 3. Documentation

- [x] 3.1 Update contributor or extension documentation with the folders to open from VSCode.
- [x] 3.2 Document the provisional project rules: virtual root `cosmo.json`, ordinary package `cosmo.json`, and package-less single-file fallback.

---
name: cosmo-release
description: Use when preparing a Cosmo release such as `/cosmo:release v0.1.0-rc1`; inspect and update release version strings, generate CHANGELOG.md from GitHub release notes, build cosmoc, and stop before pushing tags or publishing GitHub releases unless the maintainer explicitly approves.
license: Apache-2.0
compatibility: Requires Node.js, git, gh, parse-changelog, sbt, a C++17 compiler, and this repository checkout.
metadata:
  author: cosmo
  version: "1.0"
---

# Cosmo Release

Use this workflow for release requests such as `/cosmo:release v0.1.0-rc1`.

## Workflow

1. Normalize the target version.
   - Accept `v0.1.0-rc1` or `0.1.0-rc1`.
   - Use the normalized version without `v` for source files.
   - Use the tag form with `v` for changelog and release commands.

2. Inspect release readiness first.

   ```bash
   node scripts/release-preflight.mjs <version> --json
   ```

   Report pending version-string updates, current branch, changelog status, and the generated follow-up commands before making edits.

3. Local preparation is allowed.
   - Update version strings with:

     ```bash
     node scripts/release-preflight.mjs <version> --write
     ```

   - Generate or refresh the root changelog with:

     ```bash
     node scripts/generate-changelog.mjs <version> --write
     ```

   - Re-run preflight after edits.

4. Build and validate locally when feasible.
   - Fast checks:

     ```bash
     node scripts/release-preflight.mjs <version> --json
     node --test cmd/cosmo/*.test.mjs
     parse-changelog ./CHANGELOG.md
     ```

   - Full release build:

     ```bash
     COSMO_OUTPUT=target/dist/cosmoc ./scripts/build.sh
     target/dist/cosmoc --version
     ```

5. Generate release notes from the changelog and artifact directory.

   ```bash
   node scripts/draft-release.mjs <version> --artifacts target/release --output target/release-notes.md
   ```

6. Stop before external side effects.
   - Do not run `git tag`, `git push`, `gh release create`, `gh release edit`, or any publishing command unless the maintainer explicitly approves that exact action.
   - When asking for approval, show the exact command that would run.

## Guardrails

- Do not update samples, fixtures, or docs examples that intentionally use `0.0.0`.
- Keep `cosmoc --version` aligned with the release version.
- Keep root `CHANGELOG.md` in tinymist-style sections: `## vX.Y.Z - [YYYY-MM-DD]`, `### Topic`, `* item`, and a `**Full Changelog**` line.
- Use `gh api repos/<owner>/<repo>/releases/generate-notes` for changelog candidates.
- Use `parse-changelog ./CHANGELOG.md` for release notes so CI and local output agree.

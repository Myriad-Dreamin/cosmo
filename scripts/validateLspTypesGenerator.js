import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const fullMetamodelPath = "packages/lsp-types/metamodel/metaModel.full.json";
const fullOutputDir = "packages/lsp-types/src/lsp/full";

assert(
  existsSync(fullMetamodelPath),
  `${fullMetamodelPath} is missing; run: yarn fetch:lsp-types`,
);

const tempDir = mkdtempSync(join(tmpdir(), "cosmo-lsp-types-"));
try {
  execFileSync(
    "node",
    ["scripts/genLspTypesFromMetamodel.js", fullMetamodelPath, tempDir],
    { stdio: "inherit" },
  );

  for (const name of [
    "base.cos",
    "type_aliases.cos",
    "enums.cos",
    "structs.cos",
    "request.cos",
    "notification.cos",
  ]) {
    assert.equal(
      readFileSync(join(tempDir, name), "utf8"),
      readFileSync(join(fullOutputDir, name), "utf8"),
      `${join(fullOutputDir, name)} is out of date; run: yarn gen:lsp-types`,
    );
  }
} finally {
  rmSync(tempDir, { recursive: true, force: true });
}

import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { readFileSync } from "node:fs";

const generatedPath = "packages/lsp-types/src/lsp/core.cos";

const actual = execFileSync(
  "node",
  ["cmd/cosmo/main.js", "-p", "packages/lsp-types", "run"],
  {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  },
);
const expected = readFileSync(generatedPath, "utf8");

assert.equal(
  actual,
  expected,
  `${generatedPath} is out of date; run: yarn cosmo -p packages/lsp-types run > ${generatedPath}`,
);

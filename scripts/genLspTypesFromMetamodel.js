import { execFileSync } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const outputDir = process.argv[2] ?? "packages/lsp-types/src/lsp";
const generated = execFileSync(
  "node",
  ["cmd/cosmo/main.js", "-p", "packages/lsp-types", "run"],
  {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "inherit"],
  },
);

mkdirSync(outputDir, { recursive: true });

const filePattern =
  /^\/\/ @cosmo-lsp-file: ([^\n]+)\n([\s\S]*?)^\/\/ @cosmo-lsp-end\n?/gm;
let count = 0;
for (const match of generated.matchAll(filePattern)) {
  writeFileSync(join(outputDir, match[1].trim()), match[2], "utf8");
  count += 1;
}

if (count === 0) {
  throw new Error("Cosmo LSP generator produced no file markers");
}

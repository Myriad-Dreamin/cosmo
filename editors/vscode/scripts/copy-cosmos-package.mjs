import { cpSync, mkdirSync, rmSync } from "fs";
import { dirname, join, relative, resolve, sep } from "path";
import { fileURLToPath } from "url";

const scriptDir = dirname(fileURLToPath(import.meta.url));
const extensionRoot = resolve(scriptDir, "..");
const repoRoot = resolve(extensionRoot, "..", "..");
const serverRoot = join(extensionRoot, "out", "server-root");

const entries = [
  ["packages/cosmos", "packages/cosmos"],
  ["packages/cosmoc", "packages/cosmoc"],
  ["packages/ls-base", "packages/ls-base"],
  ["packages/uri-sys", "packages/uri-sys"],
  ["library/std", "library/std"],
];

rmSync(serverRoot, { force: true, recursive: true });

for (const [source, target] of entries) {
  const sourcePath = join(repoRoot, source);
  const targetPath = join(serverRoot, target);
  mkdirSync(dirname(targetPath), { recursive: true });
  cpSync(sourcePath, targetPath, {
    recursive: true,
    filter: shouldCopy,
  });
}

function shouldCopy(sourcePath) {
  const relativePath = relative(repoRoot, sourcePath);
  return !relativePath.split(sep).includes("target");
}

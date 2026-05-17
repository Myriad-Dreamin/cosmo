import { readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import path from "node:path";
import process from "node:process";

const repoRoot = process.cwd();
const workspacePath = path.join(repoRoot, "samples", "cosmo-samples.code-workspace");
const sampleSmokePath = path.join(repoRoot, "samples", "HelloWorld", "main.cos");
const generatedFolderNames = new Set([
  ".bloop",
  ".metals",
  ".scala-build",
  "cmake-build-debug",
  "dist",
  "node_modules",
  "out",
  "target",
]);

async function main() {
  const workspace = await readWorkspace();
  validateSampleSmokeFile();
  validateFolders(workspace);
  validateExcludes(workspace);
}

async function readWorkspace() {
  const text = await readFile(workspacePath, "utf8");

  try {
    return JSON.parse(text);
  } catch (error) {
    fail(`workspace JSON is invalid: ${error.message}`);
  }
}

function validateSampleSmokeFile() {
  if (existsSync(sampleSmokePath)) {
    return;
  }

  fail("samples/HelloWorld/main.cos does not exist");
}

function validateFolders(workspace) {
  if (!Array.isArray(workspace.folders)) {
    fail("workspace must define a folders array");
  }

  if (workspace.folders.length !== 1) {
    fail("workspace must open exactly one folder");
  }

  for (const folder of workspace.folders) {
    validateFolder(folder);
  }
}

function validateFolder(folder) {
  if (!folder || typeof folder.path !== "string") {
    fail("workspace folder entries must include a string path");
  }

  const folderPath = path.resolve(path.dirname(workspacePath), folder.path);
  if (!existsSync(folderPath)) {
    fail(`workspace folder does not exist: ${folder.path}`);
  }

  if (folderPath !== path.join(repoRoot, "samples")) {
    fail(`workspace folder must be samples: ${folder.path}`);
  }

  const folderName = path.basename(folderPath);
  if (generatedFolderNames.has(folderName)) {
    fail(`workspace folder must not be generated output: ${folder.path}`);
  }
}

function validateExcludes(workspace) {
  const settings = workspace.settings;
  if (!settings || typeof settings !== "object") {
    fail("workspace must define settings");
  }

  requireExcluded(settings["files.exclude"], "files.exclude");
  requireExcluded(settings["search.exclude"], "search.exclude");
}

function requireExcluded(excludes, label) {
  if (!excludes || typeof excludes !== "object") {
    fail(`workspace must define ${label}`);
  }

  for (const folderName of generatedFolderNames) {
    const pattern = `**/${folderName}`;
    if (folderName === "cmake-build-debug") {
      requirePattern(excludes, "**/cmake-build-*", label);
      continue;
    }

    requirePattern(excludes, pattern, label);
  }
}

function requirePattern(excludes, pattern, label) {
  if (excludes[pattern] === true) {
    return;
  }

  fail(`workspace ${label} must exclude ${pattern}`);
}

function fail(message) {
  console.error(`validate:vscode-workspace-sample: ${message}`);
  process.exit(1);
}

await main();

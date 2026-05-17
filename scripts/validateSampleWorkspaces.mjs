import { existsSync, readFileSync } from "node:fs";
import { readFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";

const repoRoot = process.cwd();
const samplesRoot = path.join(repoRoot, "samples");
const workspacesRoot = path.join(samplesRoot, "workspaces");
const rootManifestPath = path.join(samplesRoot, "cosmo.json");

const expectedRootMembers = [
  "workspaces/virtual-root/packages/app",
  "workspaces/virtual-root/packages/lib",
  "workspaces/package",
];
const expectedNestedMembers = ["packages/app", "packages/lib"];

async function main() {
  const rootManifest = await readJson(rootManifestPath);
  validateVirtualWorkspace(rootManifest, expectedRootMembers, rootManifestPath);

  const nestedRoot = path.join(workspacesRoot, "virtual-root", "cosmo.json");
  const nestedManifest = await readJson(nestedRoot);
  validateVirtualWorkspace(nestedManifest, expectedNestedMembers, nestedRoot);

  validatePackage("workspaces/virtual-root/packages/app", {
    name: "@cosmo/sample-virtual-app",
    sources: ["main.cos"],
  });
  validatePackage("workspaces/virtual-root/packages/lib", {
    name: "@cosmo/sample-virtual-lib",
    sources: ["lib.cos"],
  });
  validatePackage("workspaces/package", {
    name: "@cosmo/sample-package",
    sources: ["main.cos"],
  });
  validateSingleFileWorkspace();
}

async function readJson(filePath) {
  const text = await readFile(filePath, "utf8");
  try {
    return JSON.parse(text);
  } catch (error) {
    fail(`${relative(filePath)} is not valid JSON: ${error.message}`);
  }
}

function validateVirtualWorkspace(manifest, expectedMembers, manifestPath) {
  const label = relative(manifestPath);
  if (!manifest || typeof manifest !== "object" || Array.isArray(manifest)) {
    fail(`${label} must be a JSON object`);
  }
  if (!manifest.workspace || typeof manifest.workspace !== "object") {
    fail(`${label} must define workspace`);
  }
  if ("name" in manifest || "version" in manifest || "sources" in manifest) {
    fail(`${label} must be a virtual workspace root, not a package manifest`);
  }

  assertStringArray(manifest.workspace.members, `${label} workspace.members`);
  assertSameSet(manifest.workspace.members, expectedMembers, `${label} workspace.members`);

  const baseDir = path.dirname(manifestPath);
  for (const member of manifest.workspace.members) {
    const memberPath = path.resolve(baseDir, member);
    if (!existsSync(memberPath)) {
      fail(`${label} member does not exist: ${member}`);
    }
    if (!existsSync(path.join(memberPath, "cosmo.json"))) {
      fail(`${label} member is not an ordinary package: ${member}`);
    }
  }
}

function validatePackage(relativeDir, expected) {
  const packageDir = path.join(samplesRoot, relativeDir);
  const manifestPath = path.join(packageDir, "cosmo.json");
  const manifest = JSON.parse(readFileSyncUtf8(manifestPath));
  const label = relative(manifestPath);

  if (manifest.workspace) {
    fail(`${label} must be an ordinary package, not a virtual workspace root`);
  }
  if (manifest.name !== expected.name) {
    fail(`${label} must use package name ${expected.name}`);
  }
  if (manifest.version !== "0.0.0") {
    fail(`${label} must declare version 0.0.0`);
  }
  if (manifest.target !== "cosmo0") {
    fail(`${label} must target cosmo0`);
  }
  if (manifest.stageProfile !== "cosmo1.stage1") {
    fail(`${label} must use stageProfile cosmo1.stage1`);
  }
  assertStringArray(manifest.dependencies, `${label} dependencies`);
  assertStringArray(manifest.sources, `${label} sources`);
  assertSameSet(manifest.sources, expected.sources, `${label} sources`);

  for (const dependency of manifest.dependencies) {
    const dependencyPath = path.resolve(packageDir, dependency);
    if (!existsSync(path.join(dependencyPath, "cosmo.json"))) {
      fail(`${label} dependency must point to a package: ${dependency}`);
    }
  }
  for (const source of manifest.sources) {
    const sourcePath = path.join(packageDir, "src", source);
    if (!existsSync(sourcePath)) {
      fail(`${label} source does not exist under src/: ${source}`);
    }
  }
}

function validateSingleFileWorkspace() {
  const singleFileDir = path.join(workspacesRoot, "single-file");
  const singleFile = path.join(singleFileDir, "main.cos");
  if (!existsSync(singleFile)) {
    fail("samples/workspaces/single-file/main.cos must exist");
  }
  if (existsSync(path.join(singleFileDir, "cosmo.json"))) {
    fail("samples/workspaces/single-file must not define cosmo.json");
  }
  if (expectedRootMembers.includes("workspaces/single-file")) {
    fail("single-file workspace must not be a virtual workspace member package");
  }
}

function assertStringArray(value, label) {
  if (!Array.isArray(value) || !value.every((item) => typeof item === "string")) {
    fail(`${label} must be an array of strings`);
  }
}

function assertSameSet(actual, expected, label) {
  const actualSorted = [...actual].sort();
  const expectedSorted = [...expected].sort();
  if (actualSorted.length !== expectedSorted.length) {
    fail(`${label} must contain ${expectedSorted.join(", ")}`);
  }
  for (let index = 0; index < expectedSorted.length; index += 1) {
    if (actualSorted[index] !== expectedSorted[index]) {
      fail(`${label} must contain ${expectedSorted.join(", ")}`);
    }
  }
}

function readFileSyncUtf8(filePath) {
  if (!existsSync(filePath)) {
    fail(`${relative(filePath)} does not exist`);
  }
  return readFileSync(filePath, "utf8");
}

function relative(filePath) {
  return path.relative(repoRoot, filePath);
}

function fail(message) {
  console.error(`validate:sample-workspaces: ${message}`);
  process.exit(1);
}

await main();

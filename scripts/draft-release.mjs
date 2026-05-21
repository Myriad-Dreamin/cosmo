#!/usr/bin/env node

import { execFileSync } from "child_process";
import fs from "fs";
import path from "path";

const args = process.argv.slice(2);
let targetArg = "";
let artifactsDir = "target/release";
let outputPath = "target/release-notes.md";

for (let index = 0; index < args.length; index += 1) {
  const arg = args[index];
  if (arg === "--artifacts") {
    artifactsDir = args[index + 1] ?? "";
    if (!artifactsDir) {
      usage("--artifacts requires a directory");
    }
    index += 1;
    continue;
  }

  if (arg === "--output") {
    outputPath = args[index + 1] ?? "";
    if (!outputPath) {
      usage("--output requires a file path");
    }
    index += 1;
    continue;
  }

  if (!targetArg) {
    targetArg = arg;
    continue;
  }

  usage(`Unexpected argument: ${arg}`);
}

if (!targetArg) {
  usage("Missing target version");
}

const version = normalizeVersion(targetArg);
const tag = `v${version}`;
const packageVersion = JSON.parse(fs.readFileSync("package.json", "utf8")).version;

if (packageVersion !== version) {
  throw new Error(
    `package.json version (${packageVersion}) does not match release version (${version})`,
  );
}

const changelog = runText("parse-changelog", ["./CHANGELOG.md"]);
const artifactTable = renderArtifactTable(artifactsDir, tag);
const notes = `${changelog.trimEnd()}\n\n${artifactTable}\n`;

fs.mkdirSync(path.dirname(outputPath), { recursive: true });
fs.writeFileSync(outputPath, notes, "utf8");

if (process.env.GITHUB_OUTPUT) {
  fs.appendFileSync(process.env.GITHUB_OUTPUT, `tag=${tag}\n`);
}

console.log(`Wrote ${outputPath}`);

function usage(message) {
  if (message) {
    console.error(message);
  }
  console.error(
    "Usage: node scripts/draft-release.mjs <version> [--artifacts <dir>] [--output <file>]",
  );
  process.exit(1);
}

function normalizeVersion(version) {
  return version.startsWith("v") ? version.slice(1) : version;
}

function renderArtifactTable(directory, tag) {
  const artifacts = listArtifacts(directory);
  if (artifacts.length === 0) {
    return "## Artifacts\n\nNo release artifacts were found.\n";
  }

  const repository = githubRepository();
  const urlBase = `https://github.com/${repository}/releases/download/${tag}`;
  const rows = artifacts.map((artifact) => {
    const file = `[${artifact.name}](${urlBase}/${artifact.name})`;
    const checksum = artifact.sha256 ? `\`${artifact.sha256}\`` : "";
    return `| ${file} | ${checksum} |`;
  });

  return [
    "## Artifacts",
    "",
    "| File | SHA-256 |",
    "| ---- | ------- |",
    ...rows,
    "",
  ].join("\n");
}

function listArtifacts(directory) {
  if (!fs.existsSync(directory)) {
    return [];
  }

  return fs
    .readdirSync(directory)
    .filter((name) => /\.(?:tar\.gz|zip)$/.test(name))
    .sort()
    .map((name) => ({
      name,
      sha256: readChecksum(path.join(directory, `${name}.sha256`)),
    }));
}

function readChecksum(filePath) {
  if (!fs.existsSync(filePath)) {
    return "";
  }

  const content = fs.readFileSync(filePath, "utf8").trim();
  return content.split(/\s+/)[0] ?? "";
}

function runText(command, args) {
  try {
    return execFileSync(command, args, { encoding: "utf8" });
  } catch (error) {
    const stdout = error.stdout?.toString();
    if (stdout) {
      return stdout;
    }
    throw error;
  }
}

function githubRepository() {
  if (process.env.GITHUB_REPOSITORY) {
    return process.env.GITHUB_REPOSITORY;
  }

  const remote = readGitRemote();
  const match = remote.match(/github\.com[:/]([^/]+\/[^/.]+)(?:\.git)?$/);
  if (!match) {
    throw new Error(`Could not infer GitHub repository from origin: ${remote}`);
  }
  return match[1];
}

function readGitRemote() {
  try {
    return execFileSync("git", ["remote", "get-url", "origin"], {
      encoding: "utf8",
    }).trim();
  } catch (error) {
    const stdout = error.stdout?.toString().trim();
    if (stdout) {
      return stdout;
    }
    throw error;
  }
}

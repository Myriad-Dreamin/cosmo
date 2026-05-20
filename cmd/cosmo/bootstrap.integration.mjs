import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const linkedCompilerModule = resolve(
  repoRoot,
  "packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js",
);
const outputDir = resolve(repoRoot, "target/cosmoc-bootstrap-tests");

test(
  "cosmoc bootstrap reaches a deterministic v1/v2 binary fixed point",
  { timeout: 300_000 },
  () => {
    assert.ok(
      existsSync(linkedCompilerModule),
      `missing linked compiler module ${linkedCompilerModule}; run yarn compile first`,
    );
    assert.ok(
      findCxxCompiler(),
      "no C++17 compiler found; set CXX or install c++, g++, or clang++",
    );

    mkdirSync(outputDir, { recursive: true });

    const v0 = resolve(outputDir, executableName("cosmoc-v0"));
    const v1 = resolve(outputDir, executableName("cosmoc-v1"));
    const v2 = resolve(outputDir, executableName("cosmoc-v2"));

    runStep("cosmo0 builds cosmoc v0", "node", [
      "cmd/cosmo/main.js",
      "-p",
      "packages/cosmoc",
      "build",
      "-o",
      v0,
    ]);
    runStep("cosmoc v0 builds cosmoc v1", v0, [
      "-p",
      "packages/cosmoc",
      "build",
      "-o",
      v1,
    ]);
    runStep("cosmoc v1 builds cosmoc v2", v1, [
      "-p",
      "packages/cosmoc",
      "build",
      "-o",
      v2,
    ]);

    const v1Hash = sha256File(v1);
    const v2Hash = sha256File(v2);
    assert.equal(
      v2Hash,
      v1Hash,
      `cosmoc bootstrap hashes diverged\nv1 ${v1Hash} ${v1}\nv2 ${v2Hash} ${v2}`,
    );
  },
);

function runStep(label, command, args) {
  const result = spawnSync(command, args, {
    cwd: repoRoot,
    encoding: "utf8",
    env: process.env,
    maxBuffer: 32 * 1024 * 1024,
  });
  assert.equal(
    result.status,
    0,
    [
      `${label} failed`,
      `command: ${command} ${args.join(" ")}`,
      `status: ${result.status ?? "unknown"}`,
      result.stdout?.trim() ?? "",
      result.stderr?.trim() ?? "",
      result.error?.message ?? "",
    ]
      .filter(Boolean)
      .join("\n"),
  );
}

function sha256File(path) {
  return createHash("sha256").update(readFileSync(path)).digest("hex");
}

function executableName(name) {
  return process.platform === "win32" ? `${name}.exe` : name;
}

function findCxxCompiler() {
  const candidates = process.env.CXX
    ? [process.env.CXX]
    : ["c++", "g++", "clang++"];

  return candidates.find((command) => {
    const result = spawnSync(command, ["--version"], {
      cwd: repoRoot,
      encoding: "utf8",
    });
    return result.status === 0;
  });
}

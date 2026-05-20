import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import {
  applyEnvironmentFile,
  packageBuildBuildPaths,
  packageRunBuildPaths,
  parseEnvironmentFile,
  parseEnvironmentOptions,
  parsePackageBuildCommand,
  parsePackageRunCommand,
} from "./main.js";

test("package run parser accepts -p package run and forwards args after --", () => {
  assert.deepEqual(
    parsePackageRunCommand([
      "-p",
      "fixtures/cosmo0/package/run-smoke",
      "run",
      "--",
      "--emit",
      "types.json",
    ]),
    {
      packagePath: "fixtures/cosmo0/package/run-smoke",
      runArgs: ["--emit", "types.json"],
    },
  );
});

test("package run parser forwards args after run without requiring --", () => {
  assert.deepEqual(
    parsePackageRunCommand([
      "--package",
      "packages/cosmoc",
      "run",
      "--manifest",
      "fixtures/parser.manifest",
    ]),
    {
      packagePath: "packages/cosmoc",
      runArgs: ["--manifest", "fixtures/parser.manifest"],
    },
  );
});

test("environment option parser defaults to .env and strips explicit file flag", () => {
  assert.deepEqual(parseEnvironmentOptions(["run", "-f", ".env.prod", "main.cos"]), {
    argv: ["run", "main.cos"],
    envFile: ".env.prod",
    explicit: true,
  });

  assert.deepEqual(parseEnvironmentOptions(["--package", "pkg", "run"]), {
    argv: ["--package", "pkg", "run"],
    envFile: ".env",
    explicit: false,
  });
});

test("environment option parser preserves executable args after --", () => {
  assert.deepEqual(
    parseEnvironmentOptions([
      "--package",
      "pkg",
      "run",
      "--",
      "-f",
      "app-value",
    ]),
    {
      argv: ["--package", "pkg", "run", "--", "-f", "app-value"],
      envFile: ".env",
      explicit: false,
    },
  );
});

test("environment file parser supports dotenv values", () => {
  assert.deepEqual(
    parseEnvironmentFile(`
      # comment
      COSMO_LLVM_PATH=/opt/llvm
      export BUILD_MODE = "rel\\nwithdebinfo"
      EMPTY=
      LITERAL='a # b'
      TRAILING=enabled # comment
    `),
    {
      COSMO_LLVM_PATH: "/opt/llvm",
      BUILD_MODE: "rel\nwithdebinfo",
      EMPTY: "",
      LITERAL: "a # b",
      TRAILING: "enabled",
    },
  );
});

test("environment file application reads default and explicit files", () => {
  const root = mkdtempSync(join(tmpdir(), "cosmo-env-"));
  mkdirSync(join(root, "config"));
  writeFileSync(join(root, ".env"), "COSMO_DEFAULT=from-default\n", "utf8");
  writeFileSync(
    join(root, "config", ".env.prod"),
    "COSMO_LLVM_PATH=/toolchains/llvm\n",
    "utf8",
  );

  const env = {};
  assert.deepEqual(applyEnvironmentFile(".env", { cwd: root, env }), {
    COSMO_DEFAULT: "from-default",
  });
  assert.equal(env.COSMO_DEFAULT, "from-default");

  assert.deepEqual(
    applyEnvironmentFile("config/.env.prod", { cwd: root, env, required: true }),
    { COSMO_LLVM_PATH: "/toolchains/llvm" },
  );
  assert.equal(env.COSMO_LLVM_PATH, "/toolchains/llvm");
});

test("package run build paths stay under the selected package target directory", () => {
  assert.deepEqual(
    packageRunBuildPaths("/repo/packages/tool", "cosmo/package.tool"),
    {
      buildDir: "/repo/packages/tool/target/cosmo/package-run",
      sourcePath: "/repo/packages/tool/target/cosmo/package-run/cosmo_package.tool.cpp",
      executablePath:
        process.platform === "win32"
          ? "/repo/packages/tool/target/cosmo/package-run/cosmo_package.tool.exe"
          : "/repo/packages/tool/target/cosmo/package-run/cosmo_package.tool",
      logPath: "/repo/packages/tool/target/cosmo/package-run/cosmo_package.tool.compile.log",
    },
  );
});

test("package build parser accepts -p package build output", () => {
  assert.deepEqual(
    parsePackageBuildCommand([
      "-p",
      "packages/cosmoc",
      "build",
      "-o",
      "target/cosmoc-v0",
    ]),
    {
      packagePath: "packages/cosmoc",
      outputPath: "target/cosmoc-v0",
      buildArgs: ["-o", "target/cosmoc-v0"],
    },
  );
});

test("package build paths use package target scratch and requested executable", () => {
  assert.deepEqual(
    packageBuildBuildPaths(
      "/repo/packages/tool",
      "cosmo/package.tool",
      "/tmp/tool",
    ),
    {
      buildDir: "/repo/packages/tool/target/cosmo/package-build",
      sourcePath:
        "/repo/packages/tool/target/cosmo/package-build/cosmo_package.tool.cpp",
      executablePath: "/tmp/tool",
      logPath:
        "/repo/packages/tool/target/cosmo/package-build/cosmo_package.tool.compile.log",
    },
  );
});

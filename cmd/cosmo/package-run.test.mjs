import assert from "node:assert/strict";
import test from "node:test";

import {
  packageBuildBuildPaths,
  packageRunBuildPaths,
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

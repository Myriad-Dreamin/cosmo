import assert from "node:assert/strict";
import test from "node:test";

import {
  packageRunBuildPaths,
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
      "fixtures/parser.tsv",
    ]),
    {
      packagePath: "packages/cosmoc",
      runArgs: ["--manifest", "fixtures/parser.tsv"],
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

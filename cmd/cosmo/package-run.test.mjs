import assert from "node:assert/strict";
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import {
  applyEnvironmentFile,
  cosmoClangSysCMakeCacheArgs,
  cosmoEvalInputDefinition,
  cosmoEvalPrecompiledContextKey,
  cosmoEvalRequestForInput,
  cosmoEvalUnsupportedBackendDiagnostic,
  discoverCosmoEvalToolchain,
  findPackageRootForSource,
  packageBuildBuildPaths,
  packageRunBuildPaths,
  nativePackageCMakeSource,
  nativeGnuToolchainLinkHint,
  parseEvalBenchCommand,
  parseEvalSmokeCommand,
  parseEnvironmentFile,
  parseEnvironmentOptions,
  parsePackageBuildCommand,
  parsePackageRunCommand,
  parseSourceRunCommand,
  readPackageNativeSupportLibraries,
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

test("source run parser accepts a file and forwards args after --", () => {
  assert.deepEqual(
    parseSourceRunCommand(["run", "src/main.cos", "--", "--emit", "types.json"]),
    {
      input: "src/main.cos",
      runArgs: ["--emit", "types.json"],
    },
  );
});

test("source run parser forwards args after the file without requiring --", () => {
  assert.deepEqual(
    parseSourceRunCommand(["run", "main.cos", "--example-arg"]),
    {
      input: "main.cos",
      runArgs: ["--example-arg"],
    },
  );
});

test("eval smoke parser accepts input shape and backend", () => {
  assert.deepEqual(
    parseEvalSmokeCommand(["eval-smoke", "integer", "--backend", "clang-pch-executable"]),
    {
      inputShape: "integer",
      backend: "clang-pch-executable",
    },
  );
  assert.deepEqual(parseEvalSmokeCommand(["run", "main.cos"]), null);
});

test("eval benchmark parser controls heavy-header input", () => {
  assert.deepEqual(parseEvalBenchCommand(["eval-bench", "--without-heavy-json"]), {
    backend: "clang-pch-executable",
    includeHeavyJson: "disabled",
  });
});

test("eval request precompiled context key is stable and complete", () => {
  const toolchainIdentity = {
    llvmVersion: "21.1.8",
    clangExecutable: "/opt/llvm/bin/clang++",
    clangVersion: "clang version 21.1.8",
    manifestPath: "/repo/packages/cosmo-clang-sys/config/llvm-manifest.json",
    cachePath: "/repo/target/cosmo/llvm",
    offlineMode: false,
    targetPlatform: "linux",
    targetArchitecture: "x64",
    gnuToolchain: "/usr/local/gcc-14.3.0",
  };
  const first = cosmoEvalRequestForInput(cosmoEvalInputDefinition("standard"), {
    toolchainIdentity,
    evalSessionProfile: "test",
  });
  const second = cosmoEvalRequestForInput(cosmoEvalInputDefinition("standard"), {
    toolchainIdentity,
    evalSessionProfile: "test",
  });

  assert.equal(first.precompiledContextKey.id, second.precompiledContextKey.id);
  assert.equal(
    cosmoEvalPrecompiledContextKey({
      cxxStandard: "c++17",
      targetTriple: "",
      toolchainIdentity,
      includePaths: [],
      headers: ["<iostream>", "<string>", "<vector>"],
      imports: [],
      compileOptions: [],
      supportLibraryIdentities: [],
      evalSessionProfile: "test",
    }).id,
    first.precompiledContextKey.id,
  );
  assert.match(first.precompiledContextKey.stableJson, /"headers":\["<iostream>","<string>","<vector>"\]/);
});

test("eval precompiled context key changes for compile-affecting inputs", () => {
  const toolchainIdentity = {
    llvmVersion: "21.1.8",
    clangExecutable: "/opt/llvm/bin/clang++",
    clangVersion: "clang version 21.1.8",
    manifestPath: "/repo/packages/cosmo-clang-sys/config/llvm-manifest.json",
    cachePath: "/repo/target/cosmo/llvm",
    offlineMode: false,
    targetPlatform: "linux",
    targetArchitecture: "x64",
    gnuToolchain: "",
  };
  const base = cosmoEvalPrecompiledContextKey({
    cxxStandard: "c++17",
    targetTriple: "",
    toolchainIdentity,
    includePaths: [],
    headers: ["<iostream>"],
    imports: [],
    compileOptions: [],
    supportLibraryIdentities: [],
    evalSessionProfile: "test",
  });
  const changedHeader = cosmoEvalPrecompiledContextKey({
    cxxStandard: "c++17",
    targetTriple: "",
    toolchainIdentity,
    includePaths: [],
    headers: ["<iostream>", "<vector>"],
    imports: [],
    compileOptions: [],
    supportLibraryIdentities: [],
    evalSessionProfile: "test",
  });

  assert.notEqual(changedHeader.id, base.id);
});

test("eval rejects interpreter backend requests", () => {
  const diagnostic = cosmoEvalUnsupportedBackendDiagnostic("clangInterpreter");

  assert.equal(diagnostic.code, "cosmo.eval.unsupported-interpreter-backend");
  assert.match(diagnostic.message, /ordinary Clang/);
  assert.equal(cosmoEvalUnsupportedBackendDiagnostic("clang-pch-executable"), null);
});

test("eval toolchain discovery reports missing configured Clang", () => {
  const toolchain = discoverCosmoEvalToolchain({
    env: {
      COSMO_EVAL_CXX: "/not/a/real/clang++",
      COSMO_LLVM_VERSION: "21.1.8",
    },
    allowFallback: false,
  });

  assert.equal(toolchain.available, false);
  assert.equal(
    toolchain.diagnostics[0].code,
    "cosmo.eval.missing-clang-compile-support",
  );
  assert.match(toolchain.diagnostics[0].message, /COSMO_LLVM_PATH/);
});

test("source package discovery walks upward from the selected source file", () => {
  const root = mkdtempSync(join(tmpdir(), "cosmo-source-package-"));
  mkdirSync(join(root, "src"));
  writeFileSync(join(root, "cosmo.json"), '{"name":"sample"}\n', "utf8");
  writeFileSync(join(root, "src", "main.cos"), "def main(): Unit = {}\n", "utf8");

  assert.equal(findPackageRootForSource("src/main.cos", root), root);
  assert.equal(findPackageRootForSource(join(root, "src", "main.cos")), root);
});

test("source package discovery ignores workspace-only manifests", () => {
  const root = mkdtempSync(join(tmpdir(), "cosmo-single-file-"));
  writeFileSync(
    join(root, "cosmo.json"),
    '{"workspace":{"members":["packages/app"]}}\n',
    "utf8",
  );
  writeFileSync(join(root, "main.cos"), "def main(): Unit = {}\n", "utf8");

  assert.equal(findPackageRootForSource("main.cos", root), "");
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

test("cosmo-clang-sys CMake args pass GCC toolchain configuration", () => {
  const args = cosmoClangSysCMakeCacheArgs({
    GCC_TOOLCHAIN: "/usr/local/gcc-14.3.0",
    COSMO_STATIC_GNU_RUNTIME: "OFF",
    COSMO_CMAKE_C_COMPILER: "/opt/llvm/bin/clang",
    COSMO_CMAKE_CXX_COMPILER: "/opt/llvm/bin/clang++",
  });

  assert.ok(args.includes("-DCOSMO_GCC_TOOLCHAIN=/usr/local/gcc-14.3.0"));
  assert.ok(args.includes("-DCOSMO_STATIC_GNU_RUNTIME=OFF"));
  assert.ok(args.includes("-DCMAKE_C_COMPILER=/opt/llvm/bin/clang"));
  assert.ok(args.includes("-DCMAKE_CXX_COMPILER=/opt/llvm/bin/clang++"));
});

test("cosmo-clang-sys CMake args prefer COSMO_GCC_TOOLCHAIN", () => {
  const args = cosmoClangSysCMakeCacheArgs({
    COSMO_GCC_TOOLCHAIN: "/toolchains/gcc",
    GCC_TOOLCHAIN: "/usr/local/gcc-14.3.0",
  });

  assert.ok(args.includes("-DCOSMO_GCC_TOOLCHAIN=/toolchains/gcc"));
  assert.ok(args.includes("-DCOSMO_STATIC_GNU_RUNTIME=ON"));
});

test("native link failures explain GCC toolchain configuration", () => {
  const hint = nativeGnuToolchainLinkHint(
    "undefined reference to `__isoc23_strtoll'\nundefined reference to `arc4random'",
    {},
  );

  assert.match(hint, /Set COSMO_GCC_TOOLCHAIN or GCC_TOOLCHAIN/);
  assert.match(hint, /COSMO_GCC_TOOLCHAIN=\/usr\/local\/gcc-14\.3\.0/);
});

test("native link failures mention configured GCC toolchain", () => {
  const hint = nativeGnuToolchainLinkHint(
    "/lib/libLLVM.a: version `GLIBC_2.38' not found",
    { GCC_TOOLCHAIN: "/usr/local/gcc-14.3.0" },
  );

  assert.match(hint, /Configured GCC toolchain: \/usr\/local\/gcc-14\.3\.0/);
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

test("package manifest can request native support libraries", () => {
  const root = mkdtempSync(join(tmpdir(), "cosmo-native-support-"));
  writeFileSync(
    join(root, "cosmo.json"),
    JSON.stringify({
      name: "@cosmo/compiler",
      version: "0.0.0",
      nativeSupportLibraries: ["cosmo-clang-sys", "cosmo-clang-sys"],
    }),
    "utf8",
  );

  assert.deepEqual(readPackageNativeSupportLibraries(root), ["cosmo-clang-sys"]);
});

test("native package CMake source links cosmoClang to generated executable", () => {
  const source = nativePackageCMakeSource({
    sourcePath: "/repo/packages/cosmoc/target/cosmo/package-build/main.cpp",
    executablePath: "/repo/target/cosmoc",
    jsonInclude: "/repo/target/cosmo/externals/json/single_include",
    supportLibraryLinkArguments: ["/repo/target/cosmo/support/libsupport.a"],
  });

  assert.match(source, /add_subdirectory\(.+packages\/cosmo-clang-sys/);
  assert.match(source, /target_link_libraries\(cosmoPackageExecutable PRIVATE\n    cosmoClang/);
  assert.match(source, /"\/repo\/target\/cosmo\/support\/libsupport\.a"/);
  assert.match(source, /OUTPUT_NAME "cosmoc"/);
});

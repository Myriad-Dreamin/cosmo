#!/usr/bin/env node

import { spawn, spawnSync } from "child_process";
import {
  existsSync,
  mkdirSync,
  copyFileSync,
  readFileSync,
  writeFileSync,
} from "fs";
import { basename, dirname, join, resolve } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import repl from "repl";

const linkedCompilerModule =
  "../../packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js";
const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const nativeSupportLibrariesField = "nativeSupportLibraries";
const cosmoClangSysLibrary = "cosmo-clang-sys";
const nativePackageExecutableTarget = "cosmoPackageExecutable";
const nlohmannJsonDependency = {
  repoUrl: "https://github.com/nlohmann/json.git",
  version: "v3.11.3",
  revision: "9cca280a4d0ccf0c08f47a99aa71d1b0e52f8d03",
  rootDir: "target/cosmo/externals/json",
  includeDir: "target/cosmo/externals/json/single_include",
  headerPath: "target/cosmo/externals/json/single_include/nlohmann/json.hpp",
};

class CliError extends Error {
  constructor(message, exitCode = 1) {
    super(message);
    this.exitCode = exitCode;
  }
}

export function parsePackageRunCommand(argv) {
  if (argv.length < 3) {
    return null;
  }

  const packageFlag = argv[0];
  if (packageFlag !== "-p" && packageFlag !== "--package") {
    return null;
  }

  const packagePath = argv[1];
  if (!packagePath || argv[2] !== "run") {
    return null;
  }

  const trailing = argv.slice(3);
  const runArgs = trailing[0] === "--" ? trailing.slice(1) : trailing;
  return { packagePath, runArgs };
}

export function parseEnvironmentOptions(argv) {
  const commandArgs = [];
  let envFile = ".env";
  let explicit = false;

  for (let index = 0; index < argv.length; index += 1) {
    const current = argv[index];
    if (current === "--") {
      commandArgs.push(...argv.slice(index));
      break;
    }

    if (current === "-f" || current === "--env-file") {
      const value = argv[index + 1];
      if (!value) {
        throw new CliError(`${current} requires an environment file path`);
      }
      envFile = value;
      explicit = true;
      index += 1;
      continue;
    }

    if (current.startsWith("--env-file=")) {
      const value = current.slice("--env-file=".length);
      if (!value) {
        throw new CliError("--env-file requires an environment file path");
      }
      envFile = value;
      explicit = true;
      continue;
    }

    commandArgs.push(current);
  }

  return { argv: commandArgs, envFile, explicit };
}

export function applyEnvironmentFile(
  filePath = ".env",
  {
    env = process.env,
    cwd = process.cwd(),
    required = false,
  } = {},
) {
  const resolved = resolve(cwd, filePath);
  if (!existsSync(resolved)) {
    if (required) {
      throw new CliError(`environment file not found: ${filePath}`);
    }
    return {};
  }

  const values = parseEnvironmentFile(readFileSync(resolved, "utf8"), filePath);
  for (const [key, value] of Object.entries(values)) {
    env[key] = value;
  }
  return values;
}

export function parseEnvironmentFile(source, filePath = ".env") {
  const values = {};
  const lines = source.split(/\r?\n/);

  for (let index = 0; index < lines.length; index += 1) {
    const rawLine = lines[index];
    const line = rawLine.trim();
    if (!line || line.startsWith("#")) {
      continue;
    }

    const assignment = line.startsWith("export ") ? line.slice(7).trim() : line;
    const equals = assignment.indexOf("=");
    if (equals <= 0) {
      throw new CliError(`${filePath}:${index + 1}: expected KEY=VALUE`);
    }

    const key = assignment.slice(0, equals).trim();
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(key)) {
      throw new CliError(`${filePath}:${index + 1}: invalid environment key ${key}`);
    }

    values[key] = parseEnvironmentValue(
      assignment.slice(equals + 1).trim(),
      filePath,
      index + 1,
    );
  }

  return values;
}

function parseEnvironmentValue(value, filePath, line) {
  if (!value) {
    return "";
  }

  if (value.startsWith("\"")) {
    if (!value.endsWith("\"") || value.length === 1) {
      throw new CliError(`${filePath}:${line}: unterminated double-quoted value`);
    }
    return unescapeDoubleQuotedEnvironmentValue(value.slice(1, -1));
  }

  if (value.startsWith("'")) {
    if (!value.endsWith("'") || value.length === 1) {
      throw new CliError(`${filePath}:${line}: unterminated single-quoted value`);
    }
    return value.slice(1, -1);
  }

  return stripInlineEnvironmentComment(value).trim();
}

function unescapeDoubleQuotedEnvironmentValue(value) {
  return value.replace(/\\([nrt"\\])/g, (_match, escaped) => {
    switch (escaped) {
      case "n":
        return "\n";
      case "r":
        return "\r";
      case "t":
        return "\t";
      default:
        return escaped;
    }
  });
}

function stripInlineEnvironmentComment(value) {
  const comment = value.search(/\s#/);
  return comment >= 0 ? value.slice(0, comment) : value;
}

export function parsePackageBuildCommand(argv) {
  if (argv.length < 3) {
    return null;
  }

  const packageFlag = argv[0];
  if (packageFlag !== "-p" && packageFlag !== "--package") {
    return null;
  }

  const packagePath = argv[1];
  if (!packagePath || argv[2] !== "build") {
    return null;
  }

  const buildArgs = argv.slice(3);
  return { packagePath, outputPath: parseOutputPath(buildArgs), buildArgs };
}

function parseOutputPath(args) {
  for (let index = 0; index < args.length; index += 1) {
    const current = args[index];
    if (current === "-o" || current === "--output") {
      return args[index + 1] ?? "";
    }
  }
  return "";
}

export function packageRunBuildPaths(packageRoot, moduleName) {
  const safeModuleName =
    moduleName.replace(/[^A-Za-z0-9_.-]+/g, "_") || "package";
  const buildDir = join(packageRoot, "target", "cosmo", "package-run");
  const executableName =
    process.platform === "win32" ? `${safeModuleName}.exe` : safeModuleName;

  return {
    buildDir,
    sourcePath: join(buildDir, `${safeModuleName}.cpp`),
    executablePath: join(buildDir, executableName),
    logPath: join(buildDir, `${safeModuleName}.compile.log`),
  };
}

export function packageBuildBuildPaths(packageRoot, moduleName, outputPath = "") {
  const safeModuleName =
    moduleName.replace(/[^A-Za-z0-9_.-]+/g, "_") || "package";
  const buildDir = join(packageRoot, "target", "cosmo", "package-build");
  const executableName =
    process.platform === "win32" ? `${safeModuleName}.exe` : safeModuleName;
  const executablePath = outputPath
    ? resolve(outputPath)
    : join(buildDir, executableName);

  return {
    buildDir,
    sourcePath: join(buildDir, `${safeModuleName}.cpp`),
    executablePath,
    logPath: join(buildDir, `${safeModuleName}.compile.log`),
  };
}

async function loadCompilerModule() {
  return import(linkedCompilerModule);
}

async function main(argv = process.argv.slice(2)) {
  const envOptions = parseEnvironmentOptions(argv);
  applyEnvironmentFile(envOptions.envFile, { required: envOptions.explicit });

  const packageBuild = parsePackageBuildCommand(envOptions.argv);
  if (packageBuild) {
    await runPackageBuildCommand(packageBuild.packagePath, packageBuild.outputPath);
    return;
  }

  const packageRun = parsePackageRunCommand(envOptions.argv);
  if (packageRun) {
    await runPackageCommand(packageRun.packagePath, packageRun.runArgs);
    return;
  }

  const action = envOptions.argv[0];
  const input = envOptions.argv[1];
  const cosmo = await loadCompilerModule();
  const compiler = new cosmo.Cosmo();

  if (action === "run") {
    await runSingleFile(compiler, input);
  } else if (action === "i") {
    startRepl(compiler);
  } else if (action === "parse") {
    requireInput(input, "parse");
    const inputData = readFileSync(input, "utf8");
    console.log(compiler.parseAsJson(inputData));
  } else {
    requireInput(input, "compile");
    const inputData = readFileSync(input, "utf8");
    const output = envOptions.argv[2];
    const outputData = compiler.convert(inputData);

    writeFileSync(output, outputData, "utf8");
  }
}

async function runPackageCommand(packagePath, runArgs) {
  const packageRoot = resolve(packagePath);
  const cosmo = await loadCompilerModule();
  const compiler = new cosmo.Cosmo0();
  const compiled = compiler.compileRunnablePackageForHost(packageRoot);

  if (!compiled.ok) {
    printDiagnostics(compiled.diagnostics);
    throw new CliError(`cosmo package run failed during ${compiled.status}`);
  }

  const executablePath = compilePackageExecutable(packageRoot, compiled);
  await spawnExecutable(executablePath, runArgs, packageRoot);
}

async function runPackageBuildCommand(packagePath, outputPath) {
  if (outputPath === "") {
    throw new CliError("cosmo package build requires -o <output>");
  }

  const packageRoot = resolve(packagePath);
  const cosmo = await loadCompilerModule();
  const compiler = new cosmo.Cosmo0();
  const compiled = compiler.compileRunnablePackageForHost(packageRoot);

  if (!compiled.ok) {
    printDiagnostics(compiled.diagnostics);
    throw new CliError(`cosmo package build failed during ${compiled.status}`);
  }

  const paths = packageBuildBuildPaths(packageRoot, compiled.moduleName, outputPath);
  const executablePath = compilePackageExecutable(packageRoot, compiled, paths);
  console.log(executablePath);
}

function compilePackageExecutable(
  packageRoot,
  compiled,
  paths = packageRunBuildPaths(packageRoot, compiled.moduleName),
) {
  mkdirSync(paths.buildDir, { recursive: true });
  mkdirSync(dirname(paths.executablePath), { recursive: true });
  writeFileSync(paths.sourcePath, compiled.output, "utf8");

  ensureNlohmannJsonDependency();
  const supportLibraryLinkArguments = compiled.supportLibraryLinkArguments ?? [];
  ensureSupportLibraries(supportLibraryLinkArguments);
  const jsonInclude = resolve(nlohmannJsonDependency.includeDir);
  const nativeSupportLibraries = readPackageNativeSupportLibraries(packageRoot);
  if (nativeSupportLibraries.length > 0) {
    return compilePackageExecutableWithCMake(
      paths,
      jsonInclude,
      supportLibraryLinkArguments,
      nativeSupportLibraries,
    );
  }

  const compiler = findCxxCompiler();
  if (!compiler) {
    throw new CliError(
      "cosmo package run could not find a C++17 compiler; set CXX or install c++, g++, or clang++",
    );
  }

  const compileArgs = [
    "-std=c++17",
    `-I${jsonInclude}`,
    paths.sourcePath,
    ...supportLibraryLinkArguments,
    "-o",
    paths.executablePath,
  ];
  const result = spawnSync(compiler, compileArgs, { encoding: "utf8" });

  if (result.status === 0) {
    return paths.executablePath;
  }

  const log = [
    `command: ${compiler} ${compileArgs.join(" ")}`,
    `status: ${result.status ?? "unknown"}`,
    result.stdout?.trim() ?? "",
    result.stderr?.trim() ?? "",
    result.error?.message ?? "",
  ]
    .filter(Boolean)
    .join("\n");
  writeFileSync(paths.logPath, log, "utf8");
  throw new CliError(
    `cosmo package run C++ compile failed; log written to ${paths.logPath}`,
  );
}

export function readPackageNativeSupportLibraries(packageRoot) {
  const manifestPath = join(packageRoot, "cosmo.json");
  if (!existsSync(manifestPath)) {
    return [];
  }

  let parsed;
  try {
    parsed = JSON.parse(readFileSync(manifestPath, "utf8"));
  } catch (error) {
    throw new CliError(
      `could not read package native support metadata at ${manifestPath}: ${error.message}`,
    );
  }

  const value = parsed?.[nativeSupportLibrariesField];
  if (value == null) {
    return [];
  }

  if (!Array.isArray(value) || !value.every((item) => typeof item === "string")) {
    throw new CliError(
      `${manifestPath} field ${nativeSupportLibrariesField} must be an array of strings`,
    );
  }

  return [...new Set(value)];
}

function compilePackageExecutableWithCMake(
  paths,
  jsonInclude,
  supportLibraryLinkArguments,
  nativeSupportLibraries,
) {
  validateNativeSupportLibraries(nativeSupportLibraries);

  const cmake = findCmakeCommand();
  if (!cmake) {
    throw new CliError(
      "cosmo package run could not find CMake; install cmake to build native support libraries",
    );
  }

  const cmakeProjectDir = join(paths.buildDir, "native-link");
  const cmakeBuildDir = join(paths.buildDir, "native-link-build");
  mkdirSync(cmakeProjectDir, { recursive: true });
  writeFileSync(
    join(cmakeProjectDir, "CMakeLists.txt"),
    nativePackageCMakeSource({
      sourcePath: paths.sourcePath,
      executablePath: paths.executablePath,
      jsonInclude,
      supportLibraryLinkArguments,
    }),
    "utf8",
  );

  const configureArgs = [
    "-S",
    cmakeProjectDir,
    "-B",
    cmakeBuildDir,
    `-DCMAKE_BUILD_TYPE=${process.env.CMAKE_BUILD_TYPE ?? "RelWithDebInfo"}`,
    ...cosmoClangSysCMakeCacheArgs(),
  ];
  const configure = spawnSync(cmake, configureArgs, { encoding: "utf8" });
  if (configure.status !== 0) {
    writeNativeBuildLog(paths.logPath, cmake, configureArgs, configure);
    throw new CliError(
      `cosmo package run native CMake configure failed; log written to ${paths.logPath}`,
    );
  }

  const buildArgs = ["--build", cmakeBuildDir, "--target", nativePackageExecutableTarget];
  const build = spawnSync(cmake, buildArgs, { encoding: "utf8" });
  if (build.status === 0) {
    return paths.executablePath;
  }

  writeNativeBuildLog(paths.logPath, cmake, buildArgs, build);
  throw new CliError(
    `cosmo package run native CMake build failed; log written to ${paths.logPath}`,
  );
}

export function nativePackageCMakeSource({
  sourcePath,
  executablePath,
  jsonInclude,
  supportLibraryLinkArguments = [],
}) {
  const linkLibraries = [
    "cosmoClang",
    ...supportLibraryLinkArguments.map((argument) =>
      cmakeStringLiteral(resolve(argument)),
    ),
  ];
  const outputDir = dirname(executablePath);
  const outputName = basename(executablePath);

  return [
    "cmake_minimum_required(VERSION 3.19)",
    "project(cosmo_package_build LANGUAGES C CXX)",
    "",
    "set(CMAKE_CXX_STANDARD 17)",
    "set(CMAKE_CXX_STANDARD_REQUIRED ON)",
    "set(CMAKE_CXX_EXTENSIONS OFF)",
    "",
    `add_subdirectory(${cmakeStringLiteral(join(repoRoot, "packages", "cosmo-clang-sys"))} ${cmakeStringLiteral("${CMAKE_CURRENT_BINARY_DIR}/packages-cosmo-clang-sys")})`,
    "",
    `add_executable(${nativePackageExecutableTarget} ${cmakeStringLiteral(sourcePath)})`,
    `target_include_directories(${nativePackageExecutableTarget} PRIVATE ${cmakeStringLiteral(jsonInclude)})`,
    `target_link_libraries(${nativePackageExecutableTarget} PRIVATE`,
    ...linkLibraries.map((library) => `    ${library}`),
    ")",
    `set_target_properties(${nativePackageExecutableTarget} PROPERTIES`,
    `    RUNTIME_OUTPUT_DIRECTORY ${cmakeStringLiteral(outputDir)}`,
    `    RUNTIME_OUTPUT_DIRECTORY_DEBUG ${cmakeStringLiteral(outputDir)}`,
    `    RUNTIME_OUTPUT_DIRECTORY_RELEASE ${cmakeStringLiteral(outputDir)}`,
    `    RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${cmakeStringLiteral(outputDir)}`,
    `    RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL ${cmakeStringLiteral(outputDir)}`,
    `    OUTPUT_NAME ${cmakeStringLiteral(outputName)}`,
    ")",
    "",
  ].join("\n");
}

function validateNativeSupportLibraries(libraries) {
  const unsupported = libraries.filter((library) => library !== cosmoClangSysLibrary);
  if (unsupported.length === 0) {
    return;
  }

  throw new CliError(
    `unsupported native support libraries: ${unsupported.join(", ")}`,
  );
}

function cosmoClangSysCMakeCacheArgs() {
  const values = {
    COSMO_LLVM_PATH: process.env.COSMO_LLVM_PATH ?? "",
    COSMO_LLVM_VERSION: process.env.COSMO_LLVM_VERSION ?? "21.1.8",
    COSMO_LLVM_DOWNLOAD_DIR: join(repoRoot, "target", "cosmo", "llvm"),
    COSMO_LLVM_MANIFEST_PATH: join(
      repoRoot,
      "packages",
      "cosmo-clang-sys",
      "config",
      "llvm-manifest.json",
    ),
    COSMO_LLVM_ENABLE_LTO: process.env.COSMO_LLVM_ENABLE_LTO ?? "OFF",
    COSMO_LLVM_OFFLINE: process.env.COSMO_LLVM_OFFLINE ?? "OFF",
    COSMO_LLVM_TARGET_PLATFORM: process.env.COSMO_LLVM_TARGET_PLATFORM ?? "",
    COSMO_LLVM_TARGET_ARCH: process.env.COSMO_LLVM_TARGET_ARCH ?? "",
  };

  return Object.entries(values).map(([key, value]) => {
    return `-D${key}=${value}`;
  });
}

function writeNativeBuildLog(logPath, command, args, result) {
  const log = [
    `command: ${command} ${args.join(" ")}`,
    `status: ${result.status ?? "unknown"}`,
    result.stdout?.trim() ?? "",
    result.stderr?.trim() ?? "",
    result.error?.message ?? "",
  ]
    .filter(Boolean)
    .join("\n");
  writeFileSync(logPath, log, "utf8");
}

function cmakeStringLiteral(value) {
  const text = value.replaceAll("\\", "/").replaceAll('"', '\\"');
  return `"${text}"`;
}

function ensureSupportLibraries(linkArguments) {
  for (const argument of linkArguments) {
    if (existsSync(argument)) {
      continue;
    }

    const id = supportLibraryIdFromPath(argument);
    const target = `cosmo_${id.replaceAll("-", "_")}`;
    const result = spawnSync("cargo", ["build", "--release", "-p", id], {
      encoding: "utf8",
    });
    if (result.status !== 0) {
      throw new CliError(
        [`cargo build --release -p ${id} failed`, result.stdout, result.stderr]
          .filter(Boolean)
          .join("\n"),
      );
    }

    const source = join("target", "release", `lib${target}.a`);
    if (!existsSync(source)) {
      throw new CliError(`support library build did not produce ${source}`);
    }
    mkdirSync(dirname(argument), { recursive: true });
    copyFileSync(source, argument);
  }
}

function supportLibraryIdFromPath(path) {
  const parent = basename(dirname(path));
  if (!/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/.test(parent)) {
    throw new CliError(`cannot infer support-library id from ${path}`);
  }
  return parent;
}

export function ensureNlohmannJsonDependency() {
  if (existsSync(nlohmannJsonDependency.headerPath)) {
    return nlohmannJsonDependency.includeDir;
  }

  mkdirSync("target/cosmo/externals", { recursive: true });
  if (
    existsSync(nlohmannJsonDependency.rootDir) &&
    !existsSync(`${nlohmannJsonDependency.rootDir}/.git`)
  ) {
    throw new CliError(
      `nlohmann/json dependency path exists but is not a git checkout: ${nlohmannJsonDependency.rootDir}`,
    );
  }

  if (!existsSync(`${nlohmannJsonDependency.rootDir}/.git`)) {
    cloneNlohmannJsonDependency();
  }
  if (!hasNlohmannJsonRevision()) {
    fetchNlohmannJsonPinnedTag();
  }

  runGit(
    [
      "-C",
      nlohmannJsonDependency.rootDir,
      "checkout",
      "--detach",
      "--force",
      nlohmannJsonDependency.revision,
    ],
    `checkout nlohmann/json ${nlohmannJsonDependency.version} (${nlohmannJsonDependency.revision})`,
  );

  if (existsSync(nlohmannJsonDependency.headerPath)) {
    return nlohmannJsonDependency.includeDir;
  }

  throw new CliError(
    `nlohmann/json checkout did not provide ${nlohmannJsonDependency.headerPath} at ${nlohmannJsonDependency.version} (${nlohmannJsonDependency.revision})`,
  );
}

function cloneNlohmannJsonDependency() {
  runGit(
    [
      "clone",
      "--filter=blob:none",
      "--depth=1",
      "--branch",
      nlohmannJsonDependency.version,
      "--single-branch",
      nlohmannJsonDependency.repoUrl,
      nlohmannJsonDependency.rootDir,
    ],
    `clone nlohmann/json ${nlohmannJsonDependency.version}`,
  );
}

function fetchNlohmannJsonPinnedTag() {
  runGit(
    [
      "-C",
      nlohmannJsonDependency.rootDir,
      "fetch",
      "--filter=blob:none",
      "--depth=1",
      "origin",
      "tag",
      nlohmannJsonDependency.version,
    ],
    `fetch nlohmann/json ${nlohmannJsonDependency.version}`,
  );
}

function hasNlohmannJsonRevision() {
  const result = spawnSync(
    "git",
    [
      "-C",
      nlohmannJsonDependency.rootDir,
      "cat-file",
      "-e",
      `${nlohmannJsonDependency.revision}^{commit}`,
    ],
    { encoding: "utf8" },
  );
  return result.status === 0;
}

function runGit(args, action) {
  const result = spawnSync("git", args, { encoding: "utf8" });
  if (result.status === 0) {
    return;
  }

  const log = [
    `failed to ${action}`,
    `command: git ${args.join(" ")}`,
    `status: ${result.status ?? "unknown"}`,
    result.stdout?.trim() ?? "",
    result.stderr?.trim() ?? "",
    result.error?.message ?? "",
  ]
    .filter(Boolean)
    .join("\n");
  throw new CliError(log);
}

function findCxxCompiler() {
  const candidates = process.env.CXX
    ? [process.env.CXX]
    : ["c++", "g++", "clang++"];

  return candidates.find((command) => {
    const result = spawnSync(command, ["--version"], { encoding: "utf8" });
    return result.status === 0;
  });
}

function findCmakeCommand() {
  const candidates = process.env.CMAKE ? [process.env.CMAKE] : ["cmake"];
  return candidates.find((command) => {
    const result = spawnSync(command, ["--version"], { encoding: "utf8" });
    return result.status === 0;
  });
}

async function runSingleFile(compiler, input) {
  requireInput(input, "run");
  compiler.loadPackageByPath("library/std");
  compiler.preloadPackage("std");
  const executable = compiler.getExecutable(input);

  if (!executable) {
    throw new CliError(`cosmo run could not build executable for ${input}`);
  }

  await spawnExecutable(executable, [], process.cwd());
}

function startRepl(compiler) {
  const showError = (result) => {
    if (result.errors) {
      for (const err of result.errors) {
        console.error(err);
      }
    }
  };

  compiler.loadPackageByPath("library/std");
  compiler.preloadPackage("std");

  const env = compiler.createEnv();
  const result = compiler.repl("import _ from std::prelude", env);
  showError(result);
  console.log("REPL started...");

  repl.start({
    prompt: "$ ",
    writer: (t) => t,
    eval(cmd, _context, _filename, callback) {
      if (cmd.trim() === "clear") {
        console.clear();
        callback(null, "");
        return;
      }

      const result = compiler.repl(cmd, env);
      showError(result);
      callback(null, result.result);
    },
  });
}

function spawnExecutable(executable, args, cwd) {
  return new Promise((resolvePromise, rejectPromise) => {
    const child = spawn(executable, args, { cwd, stdio: "inherit" });
    child.on("error", rejectPromise);
    child.on("close", (code) => {
      if (code === 0) {
        resolvePromise();
        return;
      }

      rejectPromise(new CliError(`process exited with code ${code}`, code ?? 1));
    });
  });
}

function printDiagnostics(diagnostics) {
  for (const diagnostic of diagnostics) {
    const location =
      diagnostic.fileName && diagnostic.line > 0
        ? `${diagnostic.fileName}:${diagnostic.line}:${diagnostic.column}: `
        : "";
    console.error(
      `${location}${diagnostic.severity} ${diagnostic.code}: ${diagnostic.message}`,
    );
  }
}

function requireInput(input, action) {
  if (input) {
    return;
  }

  throw new CliError(`missing input for cosmo ${action}`);
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((error) => {
    console.error(error.message ?? error);
    process.exitCode = error.exitCode ?? 1;
  });
}

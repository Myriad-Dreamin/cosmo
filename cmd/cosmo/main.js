import { spawn, spawnSync } from "child_process";
import {
  existsSync,
  mkdirSync,
  copyFileSync,
  readFileSync,
  writeFileSync,
} from "fs";
import { basename, dirname, join, resolve } from "path";
import { pathToFileURL } from "url";
import repl from "repl";

const linkedCompilerModule =
  "../../packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js";
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

async function loadCompilerModule() {
  return import(linkedCompilerModule);
}

async function main(argv = process.argv.slice(2)) {
  const packageRun = parsePackageRunCommand(argv);
  if (packageRun) {
    await runPackageCommand(packageRun.packagePath, packageRun.runArgs);
    return;
  }

  const action = argv[0];
  const input = argv[1];
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
    const output = argv[2];
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

function compilePackageExecutable(packageRoot, compiled) {
  const paths = packageRunBuildPaths(packageRoot, compiled.moduleName);
  mkdirSync(paths.buildDir, { recursive: true });
  writeFileSync(paths.sourcePath, compiled.output, "utf8");

  const compiler = findCxxCompiler();
  if (!compiler) {
    throw new CliError(
      "cosmo package run could not find a C++17 compiler; set CXX or install c++, g++, or clang++",
    );
  }

  ensureNlohmannJsonDependency();
  ensureSupportLibraries(compiled.supportLibraryLinkArguments ?? []);
  const jsonInclude = resolve(nlohmannJsonDependency.includeDir);
  const compileArgs = [
    "-std=c++17",
    `-I${jsonInclude}`,
    paths.sourcePath,
    ...compiled.supportLibraryLinkArguments,
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

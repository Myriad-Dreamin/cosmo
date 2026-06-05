#!/usr/bin/env node

import { spawn, spawnSync } from "child_process";
import { createHash } from "crypto";
import {
  existsSync,
  mkdirSync,
  readdirSync,
  copyFileSync,
  readFileSync,
  writeFileSync,
} from "fs";
import { basename, dirname, join, resolve } from "path";
import { fileURLToPath, pathToFileURL } from "url";

const linkedCompilerModule =
  "../../packages/cosmo0/target/scala-3.3.3/cosmo0-opt/main.js";
const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const nativeSupportLibrariesField = "nativeSupportLibraries";
const cosmoClangSysLibrary = "cosmo-clang-sys";
const nativePackageExecutableTarget = "cosmoPackageExecutable";
const nativeGnuToolchainMissingSymbolPattern =
  /(?:__isoc23_[A-Za-z0-9_]+|\barc4random(?:_[A-Za-z0-9_]+)?\b|GLIBC_2\.(?:3[6-9]|[4-9]\d))/;
const nlohmannJsonDependency = {
  repoUrl: "https://github.com/nlohmann/json.git",
  version: "v3.11.3",
  revision: "9cca280a4d0ccf0c08f47a99aa71d1b0e52f8d03",
  rootDir: "target/cosmo/externals/json",
  includeDir: "target/cosmo/externals/json/single_include",
  headerPath: "target/cosmo/externals/json/single_include/nlohmann/json.hpp",
};
const cosmoEvalCompiledProviderBackend = "clang-pch-executable";
const cosmoEvalUnsupportedInterpreterBackends = new Set([
  "clang-repl",
  "clangInterpreter",
  "clang-interpreter",
]);
const cosmoEvalCacheRoot = "target/cosmo/eval/cache";
const cosmoEvalBenchRoot = "target/cosmo/bench/eval";

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

export function parseSourceRunCommand(argv) {
  if (argv.length < 2 || argv[0] !== "run") {
    return null;
  }

  const input = argv[1];
  const trailing = argv.slice(2);
  const runArgs = trailing[0] === "--" ? trailing.slice(1) : trailing;
  return { input, runArgs };
}

export function parseEvalSmokeCommand(argv) {
  if (argv.length === 0 || argv[0] !== "eval-smoke") {
    return null;
  }

  const command = {
    inputShape: "all",
    backend: cosmoEvalCompiledProviderBackend,
  };
  for (let index = 1; index < argv.length; index += 1) {
    const current = argv[index];
    if (current === "--backend") {
      command.backend = requiredOptionValue(argv, index, "--backend");
      index += 1;
      continue;
    }
    if (current.startsWith("--backend=")) {
      command.backend = current.slice("--backend=".length);
      continue;
    }
    if (current === "integer" || current === "standard" || current === "all") {
      command.inputShape = current;
      continue;
    }
    throw new CliError(`unsupported cosmo eval-smoke option: ${current}`);
  }
  return command;
}

export function parseEvalBenchCommand(argv) {
  if (argv.length === 0 || argv[0] !== "eval-bench") {
    return null;
  }

  const command = {
    backend: cosmoEvalCompiledProviderBackend,
    includeHeavyJson: "auto",
  };
  for (let index = 1; index < argv.length; index += 1) {
    const current = argv[index];
    if (current === "--backend") {
      command.backend = requiredOptionValue(argv, index, "--backend");
      index += 1;
      continue;
    }
    if (current.startsWith("--backend=")) {
      command.backend = current.slice("--backend=".length);
      continue;
    }
    if (current === "--with-heavy-json") {
      command.includeHeavyJson = "required";
      continue;
    }
    if (current === "--without-heavy-json") {
      command.includeHeavyJson = "disabled";
      continue;
    }
    throw new CliError(`unsupported cosmo eval-bench option: ${current}`);
  }
  return command;
}

function requiredOptionValue(argv, index, optionName) {
  const value = argv[index + 1];
  if (!value) {
    throw new CliError(`${optionName} requires a value`);
  }
  return value;
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

function isPackageManifest(manifestPath) {
  try {
    const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
    return typeof manifest.name === "string" && manifest.name.length > 0;
  } catch {
    return true;
  }
}

export function findPackageRootForSource(sourcePath, cwd = process.cwd()) {
  if (!sourcePath) {
    return "";
  }

  let current = dirname(resolve(cwd, sourcePath));
  while (true) {
    const manifestPath = join(current, "cosmo.json");
    if (existsSync(manifestPath) && isPackageManifest(manifestPath)) {
      return current;
    }

    const parent = dirname(current);
    if (parent === current) {
      return "";
    }
    current = parent;
  }
}

async function loadCompilerModule() {
  return import(linkedCompilerModule);
}

async function main(argv = process.argv.slice(2)) {
  const envOptions = parseEnvironmentOptions(argv);
  applyEnvironmentFile(envOptions.envFile, { required: envOptions.explicit });

  const evalSmoke = parseEvalSmokeCommand(envOptions.argv);
  if (evalSmoke) {
    await runEvalSmokeCommand(evalSmoke);
    return;
  }

  const evalBench = parseEvalBenchCommand(envOptions.argv);
  if (evalBench) {
    await runEvalBenchCommand(evalBench);
    return;
  }

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

  const sourceRun = parseSourceRunCommand(envOptions.argv);
  if (sourceRun) {
    const packageRoot = findPackageRootForSource(sourceRun.input);
    if (packageRoot) {
      await runPackageCommand(packageRoot, sourceRun.runArgs);
      return;
    }

    const cosmo0 = await loadCompilerModule();
    const compiler = new cosmo0.Cosmo0();
    await runSingleFile(compiler, sourceRun.input, sourceRun.runArgs);
    return;
  }

  if (envOptions.argv[0] === "run") {
    throw new CliError("missing input for cosmo run");
  }

  throw new CliError(
    "unsupported cosmo command; use `cosmo run <file>`, `cosmo -p <package> run|build`, `cosmo eval-smoke`, or `cosmo eval-bench`",
  );
}

async function runEvalSmokeCommand(command) {
  const shapes =
    command.inputShape === "all" ? ["integer", "standard"] : [command.inputShape];
  const toolchain = discoverCosmoEvalToolchain();
  const results = shapes.map((shape) =>
    runCosmoEvalProviderInput(shape, {
      backend: command.backend,
      toolchain,
      evalSessionProfile: "smoke",
    }),
  );
  const status = results.every((result) => result.status === "succeeded")
    ? "succeeded"
    : "failed";
  const payload = results.length === 1 ? results[0] : { status, results };
  console.log(JSON.stringify(payload, null, 2));

  if (status !== "succeeded") {
    throw new CliError("cosmo eval smoke failed");
  }
}

async function runEvalBenchCommand(command) {
  const toolchain = discoverCosmoEvalToolchain();
  const generatedAt = new Date();
  const runId = generatedAt.toISOString().replace(/[:.]/g, "-");
  const shapes = cosmoEvalBenchmarkInputShapes(command);
  const inputs = [];

  for (const shape of shapes) {
    const profile = `benchmark.${shape}.${runId}`;
    const first = runCosmoEvalProviderInput(shape, {
      backend: command.backend,
      toolchain,
      evalSessionProfile: profile,
    });
    const repeated = runCosmoEvalProviderInput(shape, {
      backend: command.backend,
      toolchain,
      evalSessionProfile: profile,
    });
    inputs.push({
      inputShape: shape,
      cold: first,
      warm: repeated,
      measurementsMillis: {
        coldContextCreation: first.timingsMillis?.precompiledContext ?? 0,
        warmContextReuse: repeated.timingsMillis?.precompiledContext ?? 0,
        firstProviderEntryCompile: first.timingsMillis?.providerCompile ?? 0,
        repeatedProviderEntryCompile:
          repeated.timingsMillis?.providerCompile ?? 0,
        firstLoadInvoke: first.timingsMillis?.loadInvoke ?? 0,
        repeatedLoadInvoke: repeated.timingsMillis?.loadInvoke ?? 0,
        firstTotalRequest: first.timingsMillis?.totalRequest ?? 0,
        repeatedTotalRequest: repeated.timingsMillis?.totalRequest ?? 0,
      },
    });
  }

  const report = {
    generatedAt: generatedAt.toISOString(),
    hostPlatform: process.platform,
    hostArchitecture: process.arch,
    llvmClangVersion: toolchain.identity.clangVersion,
    compileBackendIdentity: command.backend,
    buildProfile: process.env.CMAKE_BUILD_TYPE ?? "RelWithDebInfo",
    inputShapes: shapes,
    inputs,
  };
  mkdirSync(cosmoEvalBenchRoot, { recursive: true });
  const reportPath = join(cosmoEvalBenchRoot, `eval-${runId}.json`);
  writeFileSync(reportPath, JSON.stringify(report, null, 2).concat("\n"), "utf8");
  console.log(reportPath);

  const failed = inputs.flatMap((input) => [input.cold, input.warm]).find(
    (result) => result.status !== "succeeded",
  );
  if (failed) {
    throw new CliError("cosmo eval benchmark failed");
  }
}

function cosmoEvalBenchmarkInputShapes(command) {
  const shapes = ["integer", "standard"];
  if (command.includeHeavyJson === "disabled") {
    return shapes;
  }

  if (command.includeHeavyJson === "required") {
    ensureNlohmannJsonDependency();
  }
  if (existsSync(nlohmannJsonDependency.headerPath)) {
    shapes.push("nlohmann-json");
    return shapes;
  }
  if (command.includeHeavyJson === "required") {
    throw new CliError(
      `nlohmann/json header not available at ${nlohmannJsonDependency.headerPath}`,
    );
  }
  return shapes;
}

export function runCosmoEvalProviderInput(
  inputShape,
  {
    backend = cosmoEvalCompiledProviderBackend,
    toolchain = discoverCosmoEvalToolchain(),
    evalSessionProfile = "smoke",
  } = {},
) {
  const input = cosmoEvalInputDefinition(inputShape);
  const request = cosmoEvalRequestForInput(input, {
    backend,
    toolchainIdentity: toolchain.identity,
    evalSessionProfile,
  });
  return executeCosmoEvalRequest(request, input, toolchain);
}

export function cosmoEvalRequestForInput(
  input,
  {
    backend = cosmoEvalCompiledProviderBackend,
    toolchainIdentity = cosmoEvalUnknownToolchainIdentity(),
    evalSessionProfile = "smoke",
  } = {},
) {
  const target = {
    cxxStandard: "c++17",
    targetTriple: process.env.COSMO_EVAL_TARGET_TRIPLE ?? "",
    buildProfile: evalSessionProfile,
  };
  const precompiledContextKey = cosmoEvalPrecompiledContextKey({
    cxxStandard: target.cxxStandard,
    targetTriple: target.targetTriple,
    toolchainIdentity,
    includePaths: input.includePaths,
    headers: input.headers,
    imports: input.imports,
    compileOptions: input.compileOptions,
    supportLibraryIdentities: input.supportLibraryIdentities,
    evalSessionProfile,
  });

  return {
    compilerId: "cosmo0",
    evalIdentity: `smoke.${input.inputShape}`,
    providerIdentity: `cosmo.eval.${input.inputShape}`,
    serializedInput: input.serializedInput,
    imports: input.imports,
    headers: input.headers,
    includePaths: input.includePaths,
    librarySearchPaths: [],
    compileOptions: input.compileOptions,
    generatedEntrySource: input.generatedEntrySource,
    target,
    resourceLimits: {
      timeoutMillis: 10000,
      maxOutputBytes: 64 * 1024,
    },
    precompiledContextKey,
    toolchainIdentity,
    backend,
    requestedFacts: input.requestedFacts,
    supportLibraryIdentities: input.supportLibraryIdentities,
  };
}

export function cosmoEvalPrecompiledContextKey(input) {
  const stable = stableJson({
    compileOptions: input.compileOptions,
    cxxStandard: input.cxxStandard,
    evalSessionProfile: input.evalSessionProfile,
    headers: input.headers,
    imports: input.imports,
    includePaths: input.includePaths,
    supportLibraryIdentities: input.supportLibraryIdentities,
    targetTriple: input.targetTriple,
    toolchainIdentity: input.toolchainIdentity,
  });
  return {
    stableJson: stable,
    id: createHash("sha256").update(stable).digest("hex").slice(0, 24),
  };
}

export function cosmoEvalInputDefinition(inputShape) {
  if (inputShape === "integer") {
    return {
      inputShape,
      serializedInput: "1+1",
      imports: [],
      headers: ["<iostream>"],
      includePaths: [],
      compileOptions: [],
      supportLibraryIdentities: [],
      requestedFacts: ["return:i32"],
      generatedEntrySource:
        'extern "C" int cosmo_eval_provider_entry() { return 1 + 1; }',
      expectedOutput: "2",
    };
  }

  if (inputShape === "standard") {
    return {
      inputShape,
      serializedInput: "std::string + std::vector",
      imports: [],
      headers: ["<iostream>", "<string>", "<vector>"],
      includePaths: [],
      compileOptions: [],
      supportLibraryIdentities: [],
      requestedFacts: ["return:i32", "std:string", "std:vector"],
      generatedEntrySource: [
        'extern "C" int cosmo_eval_provider_entry() {',
        '  std::string label = "cosmo";',
        "  std::vector<int> values = {1, 2, 3};",
        "  return static_cast<int>(label.size() + values.size());",
        "}",
      ].join("\n"),
      expectedOutput: "8",
    };
  }

  if (inputShape === "nlohmann-json") {
    return {
      inputShape,
      serializedInput: '{"answer":42}',
      imports: [],
      headers: ["<iostream>", "<nlohmann/json.hpp>"],
      includePaths: [resolve(nlohmannJsonDependency.includeDir)],
      compileOptions: [],
      supportLibraryIdentities: ["nlohmann-json"],
      requestedFacts: ["return:i32", "nlohmann:json"],
      generatedEntrySource: [
        'extern "C" int cosmo_eval_provider_entry() {',
        '  nlohmann::json value = nlohmann::json::parse("{\\"answer\\":42}");',
        '  return value["answer"].get<int>();',
        "}",
      ].join("\n"),
      expectedOutput: "42",
    };
  }

  throw new CliError(`unknown cosmo eval input shape: ${inputShape}`);
}

function executeCosmoEvalRequest(request, input, toolchain) {
  const totalStart = process.hrtime.bigint();
  const unsupportedBackend = cosmoEvalUnsupportedBackendDiagnostic(
    request.backend,
  );
  if (unsupportedBackend) {
    return cosmoEvalUnsupportedResult(request, [unsupportedBackend], totalStart);
  }
  if (!toolchain.available) {
    return cosmoEvalUnsupportedResult(request, toolchain.diagnostics, totalStart);
  }

  const context = ensureCosmoEvalPrecompiledContext(request, toolchain);
  if (context.result) {
    return context.result;
  }

  const compile = compileCosmoEvalProvider(request, toolchain, context);
  if (compile.result) {
    return compile.result;
  }

  const loadStart = process.hrtime.bigint();
  const run = spawnSync(compile.executablePath, [], {
    encoding: "utf8",
    maxBuffer: request.resourceLimits.maxOutputBytes,
    timeout: request.resourceLimits.timeoutMillis,
  });
  const loadInvokeMillis = elapsedMillis(loadStart);
  if (run.status !== 0) {
    return cosmoEvalFailedResult(
      request,
      "cosmo.eval.provider-invoke-failed",
      cosmoEvalCommandFailureMessage(
        compile.executablePath,
        [],
        run,
        "provider entry invocation failed",
      ),
      context.cacheSummary,
      totalStart,
      {
        precompiledContext: context.elapsedMillis,
        providerCompile: compile.elapsedMillis,
        loadInvoke: loadInvokeMillis,
      },
      compile.artifacts,
      run.stdout ?? "",
      run.stderr ?? "",
    );
  }

  const serializedOutput = (run.stdout ?? "").trim();
  if (serializedOutput !== input.expectedOutput) {
    return cosmoEvalFailedResult(
      request,
      "cosmo.eval.unexpected-output",
      `provider entry returned ${JSON.stringify(serializedOutput)}; expected ${JSON.stringify(input.expectedOutput)}`,
      context.cacheSummary,
      totalStart,
      {
        precompiledContext: context.elapsedMillis,
        providerCompile: compile.elapsedMillis,
        loadInvoke: loadInvokeMillis,
      },
      compile.artifacts,
      run.stdout ?? "",
      run.stderr ?? "",
    );
  }

  return {
    status: "succeeded",
    diagnostics: [],
    serializedOutput,
    requestedCppFacts: request.requestedFacts,
    supportBindingMetadata: request.supportLibraryIdentities,
    artifacts: compile.artifacts,
    cacheSummary: context.cacheSummary,
    capturedOutput: cosmoEvalCapturedOutput(run.stdout ?? "", run.stderr ?? ""),
    timingsMillis: cosmoEvalTimings(totalStart, {
      precompiledContext: context.elapsedMillis,
      providerCompile: compile.elapsedMillis,
      loadInvoke: loadInvokeMillis,
    }),
    request: cosmoEvalPublicRequestSummary(request),
  };
}

function ensureCosmoEvalPrecompiledContext(request, toolchain) {
  const start = process.hrtime.bigint();
  const key = request.precompiledContextKey;
  const contextDir = join(cosmoEvalCacheRoot, key.id);
  const contextHeaderPath = join(contextDir, "context.hpp");
  const metadataPath = join(contextDir, "context.json");
  const pchPath = join(contextDir, "context.hpp.pch");
  mkdirSync(contextDir, { recursive: true });

  const contextSource = cosmoEvalContextHeaderSource(request);
  const metadata = stableJson({
    contextSource,
    key: key.stableJson,
    contextHeaderPath,
    pchPath,
  });
  const existingMetadata = existsSync(metadataPath)
    ? readFileSync(metadataPath, "utf8")
    : "";
  const cacheState =
    existsSync(pchPath) && existingMetadata === metadata
      ? "reused"
      : existingMetadata
        ? "invalidated"
        : "created";

  if (cacheState !== "reused") {
    writeFileSync(contextHeaderPath, contextSource, "utf8");
    const args = [
      `-std=${request.target.cxxStandard}`,
      ...cosmoEvalTargetArgs(request),
      ...cosmoEvalIncludeArgs(request.includePaths),
      ...request.compileOptions,
      "-x",
      "c++-header",
      contextHeaderPath,
      "-o",
      pchPath,
    ];
    const pch = spawnSync(toolchain.command, args, {
      encoding: "utf8",
      maxBuffer: 8 * 1024 * 1024,
    });
    if (pch.status !== 0) {
      const cacheSummary = cosmoEvalCacheSummary(
        key.id,
        cacheState,
        pchPath,
        "precompiled context build failed",
      );
      return {
        result: cosmoEvalFailedResult(
          request,
          "cosmo.eval.pch-compile-failed",
          cosmoEvalCommandFailureMessage(
            toolchain.command,
            args,
            pch,
            "precompiled context compile failed",
          ),
          cacheSummary,
          start,
          { precompiledContext: elapsedMillis(start) },
          [],
          pch.stdout ?? "",
          pch.stderr ?? "",
        ),
      };
    }
    writeFileSync(metadataPath, metadata, "utf8");
  }

  return {
    cacheSummary: cosmoEvalCacheSummary(
      key.id,
      cacheState,
      pchPath,
      `precompiled context ${cacheState}`,
    ),
    contextHeaderPath,
    pchPath,
    elapsedMillis: elapsedMillis(start),
  };
}

function compileCosmoEvalProvider(request, toolchain, context) {
  const start = process.hrtime.bigint();
  const contextDir = dirname(context.contextHeaderPath);
  const sourcePath = join(contextDir, "provider_entry.cpp");
  const executablePath = join(
    contextDir,
    cosmoEvalExecutableName("provider_entry"),
  );
  writeFileSync(sourcePath, cosmoEvalProviderSource(request), "utf8");

  const args = [
    `-std=${request.target.cxxStandard}`,
    ...cosmoEvalTargetArgs(request),
    ...cosmoEvalIncludeArgs(request.includePaths),
    ...request.compileOptions,
    "-include-pch",
    context.pchPath,
    sourcePath,
    "-o",
    executablePath,
  ];
  const compile = spawnSync(toolchain.command, args, {
    encoding: "utf8",
    maxBuffer: 8 * 1024 * 1024,
  });
  const artifacts = [
    cosmoEvalArtifactSummary("precompiled-context", context.pchPath),
    cosmoEvalArtifactSummary("provider-source", sourcePath),
    cosmoEvalArtifactSummary("provider-executable", executablePath),
  ];
  if (compile.status !== 0) {
    return {
      result: cosmoEvalFailedResult(
        request,
        "cosmo.eval.provider-compile-failed",
        cosmoEvalCommandFailureMessage(
          toolchain.command,
          args,
          compile,
          "provider entry compile failed",
        ),
        context.cacheSummary,
        start,
        {
          precompiledContext: context.elapsedMillis,
          providerCompile: elapsedMillis(start),
        },
        artifacts,
        compile.stdout ?? "",
        compile.stderr ?? "",
      ),
    };
  }

  return {
    executablePath,
    artifacts,
    elapsedMillis: elapsedMillis(start),
  };
}

function cosmoEvalContextHeaderSource(request) {
  return request.headers.map(cosmoEvalIncludeLine).join("\n").concat("\n");
}

function cosmoEvalProviderSource(request) {
  return [
    request.generatedEntrySource,
    "",
    "int main() {",
    "  std::cout << cosmo_eval_provider_entry() << \"\\n\";",
    "  return 0;",
    "}",
    "",
  ].join("\n");
}

function cosmoEvalIncludeLine(header) {
  if (header.startsWith("<") || header.startsWith('"')) {
    return `#include ${header}`;
  }
  return `#include "${header.replaceAll("\\", "\\\\").replaceAll('"', '\\"')}"`;
}

function cosmoEvalIncludeArgs(includePaths) {
  return includePaths.map((includePath) => `-I${includePath}`);
}

function cosmoEvalTargetArgs(request) {
  const args = [];
  if (request.target.targetTriple) {
    args.push(`--target=${request.target.targetTriple}`);
  }
  if (request.toolchainIdentity.gnuToolchain) {
    args.push(`--gcc-toolchain=${request.toolchainIdentity.gnuToolchain}`);
  }
  return args;
}

export function cosmoEvalUnsupportedBackendDiagnostic(backend) {
  if (!cosmoEvalUnsupportedInterpreterBackends.has(backend)) {
    return null;
  }
  return {
    severity: "error",
    code: "cosmo.eval.unsupported-interpreter-backend",
    message:
      "clang-repl and clangInterpreter are not supported eval backends; provider entry functions must compile through ordinary Clang",
  };
}

export function discoverCosmoEvalToolchain({
  env = process.env,
  allowFallback = true,
} = {}) {
  for (const command of cosmoEvalClangCandidates(env, { allowFallback })) {
    const version = spawnSync(command, ["--version"], {
      encoding: "utf8",
      maxBuffer: 1024 * 1024,
    });
    if (version.status !== 0) {
      continue;
    }
    const versionOutput = [version.stdout, version.stderr].filter(Boolean).join("\n");
    if (!/clang/i.test(versionOutput)) {
      continue;
    }
    return {
      available: true,
      command,
      identity: cosmoEvalToolchainIdentity(command, versionOutput, env),
      diagnostics: [],
    };
  }

  const identity = cosmoEvalUnknownToolchainIdentity(env);
  return {
    available: false,
    command: "",
    identity,
    diagnostics: [
      {
        severity: "error",
        code: "cosmo.eval.missing-clang-compile-support",
        message:
          "cosmo eval requires Clang compile, PCH/precompiled context, link, and load support; set COSMO_EVAL_CXX or COSMO_LLVM_PATH, or populate the repository LLVM manifest cache under target/cosmo/llvm",
      },
    ],
  };
}

function cosmoEvalClangCandidates(env, { allowFallback = true } = {}) {
  const candidates = [];
  const configuredEvalCxx = firstNonEmptyEnvironmentValue(env, "COSMO_EVAL_CXX");
  if (configuredEvalCxx) {
    candidates.push(configuredEvalCxx);
  }

  const llvmPath = firstNonEmptyEnvironmentValue(env, "COSMO_LLVM_PATH");
  if (llvmPath) {
    candidates.push(join(llvmPath, "bin", cosmoEvalExecutableName("clang++")));
  }
  if (allowFallback) {
    candidates.push(...cosmoEvalCachedClangCandidates(env));

    const cxx = firstNonEmptyEnvironmentValue(env, "CXX");
    if (cxx && basename(cxx).includes("clang")) {
      candidates.push(cxx);
    }
    candidates.push(cosmoEvalExecutableName("clang++"));
  }

  return [...new Set(candidates.filter(Boolean))];
}

function cosmoEvalCachedClangCandidates(env) {
  const version = env.COSMO_LLVM_VERSION ?? "21.1.8";
  const root = join(repoRoot, "target", "cosmo", "llvm", version);
  if (!existsSync(root)) {
    return [];
  }
  return readdirSync(root).map((entry) =>
    join(root, entry, "bin", cosmoEvalExecutableName("clang++")),
  );
}

function cosmoEvalToolchainIdentity(command, versionOutput, env = process.env) {
  return {
    llvmVersion: env.COSMO_LLVM_VERSION ?? "21.1.8",
    clangExecutable: command,
    clangVersion: versionOutput.split(/\r?\n/).find(Boolean) ?? "",
    manifestPath: join(
      repoRoot,
      "packages",
      "cosmo-clang-sys",
      "config",
      "llvm-manifest.json",
    ),
    cachePath: join(repoRoot, "target", "cosmo", "llvm"),
    offlineMode: (env.COSMO_LLVM_OFFLINE ?? "OFF").toUpperCase() === "ON",
    targetPlatform: env.COSMO_LLVM_TARGET_PLATFORM ?? process.platform,
    targetArchitecture: env.COSMO_LLVM_TARGET_ARCH ?? process.arch,
    gnuToolchain: firstNonEmptyEnvironmentValue(
      env,
      "COSMO_GCC_TOOLCHAIN",
      "GCC_TOOLCHAIN",
    ),
  };
}

function cosmoEvalUnknownToolchainIdentity(env = process.env) {
  return {
    llvmVersion: env.COSMO_LLVM_VERSION ?? "21.1.8",
    clangExecutable: "",
    clangVersion: "",
    manifestPath: join(
      repoRoot,
      "packages",
      "cosmo-clang-sys",
      "config",
      "llvm-manifest.json",
    ),
    cachePath: join(repoRoot, "target", "cosmo", "llvm"),
    offlineMode: (env.COSMO_LLVM_OFFLINE ?? "OFF").toUpperCase() === "ON",
    targetPlatform: env.COSMO_LLVM_TARGET_PLATFORM ?? process.platform,
    targetArchitecture: env.COSMO_LLVM_TARGET_ARCH ?? process.arch,
    gnuToolchain: firstNonEmptyEnvironmentValue(
      env,
      "COSMO_GCC_TOOLCHAIN",
      "GCC_TOOLCHAIN",
    ),
  };
}

function cosmoEvalUnsupportedResult(request, diagnostics, totalStart) {
  return {
    status: "unsupported",
    diagnostics,
    serializedOutput: null,
    requestedCppFacts: [],
    supportBindingMetadata: [],
    artifacts: [],
    cacheSummary: cosmoEvalCacheSummary(
      request.precompiledContextKey.id,
      "disabled",
      "",
      "eval request did not reach provider compilation",
    ),
    capturedOutput: cosmoEvalCapturedOutput("", ""),
    timingsMillis: cosmoEvalTimings(totalStart, {}),
    request: cosmoEvalPublicRequestSummary(request),
  };
}

function cosmoEvalFailedResult(
  request,
  code,
  message,
  cacheSummary,
  totalStart,
  timings,
  artifacts = [],
  stdout = "",
  stderr = "",
) {
  return {
    status: "failed",
    diagnostics: [{ severity: "error", code, message }],
    serializedOutput: null,
    requestedCppFacts: request.requestedFacts,
    supportBindingMetadata: request.supportLibraryIdentities,
    artifacts,
    cacheSummary,
    capturedOutput: cosmoEvalCapturedOutput(stdout, stderr),
    timingsMillis: cosmoEvalTimings(totalStart, timings),
    request: cosmoEvalPublicRequestSummary(request),
  };
}

function cosmoEvalCacheSummary(keyId, state, artifactPath, message) {
  return { keyId, state, artifactPath, message };
}

function cosmoEvalCapturedOutput(stdout, stderr) {
  return {
    stdoutBytes: Buffer.byteLength(stdout),
    stderrBytes: Buffer.byteLength(stderr),
    stdoutPreview: stdout.slice(0, 4096),
    stderrPreview: stderr.slice(0, 4096),
  };
}

function cosmoEvalTimings(totalStart, timings) {
  return {
    precompiledContext: timings.precompiledContext ?? 0,
    providerCompile: timings.providerCompile ?? 0,
    loadInvoke: timings.loadInvoke ?? 0,
    totalRequest: elapsedMillis(totalStart),
  };
}

function cosmoEvalPublicRequestSummary(request) {
  return {
    compilerId: request.compilerId,
    evalIdentity: request.evalIdentity,
    providerIdentity: request.providerIdentity,
    headers: request.headers,
    includePaths: request.includePaths,
    target: request.target,
    backend: request.backend,
    precompiledContextKey: {
      id: request.precompiledContextKey.id,
    },
    toolchainIdentity: request.toolchainIdentity,
  };
}

function cosmoEvalArtifactSummary(kind, path) {
  return {
    kind,
    path,
    sha256: existsSync(path)
      ? createHash("sha256").update(readFileSync(path)).digest("hex")
      : "",
  };
}

function cosmoEvalCommandFailureMessage(command, args, result, heading) {
  return [
    heading,
    `command: ${command} ${args.join(" ")}`,
    `status: ${result.status ?? "unknown"}`,
    result.stdout?.trim() ?? "",
    result.stderr?.trim() ?? "",
    result.error?.message ?? "",
  ]
    .filter(Boolean)
    .join("\n");
}

function stableJson(value) {
  if (value === null) {
    return "null";
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  if (typeof value === "object") {
    return `{${Object.keys(value)
      .sort()
      .map((key) => `${JSON.stringify(key)}:${stableJson(value[key])}`)
      .join(",")}}`;
  }
  return JSON.stringify(value);
}

function elapsedMillis(start) {
  return Number(process.hrtime.bigint() - start) / 1_000_000;
}

function cosmoEvalExecutableName(name) {
  return process.platform === "win32" ? `${name}.exe` : name;
}

async function runPackageCommand(packagePath, runArgs) {
  const packageRoot = resolve(packagePath);
  const cosmo0 = await loadCompilerModule();
  const compiler = new cosmo0.Cosmo0();
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
  const cosmo0 = await loadCompilerModule();
  const compiler = new cosmo0.Cosmo0();
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
    const log = writeNativeBuildLog(paths.logPath, cmake, configureArgs, configure);
    throw new CliError(
      nativeBuildFailureMessage("configure", paths.logPath, log),
    );
  }

  const buildArgs = ["--build", cmakeBuildDir, "--target", nativePackageExecutableTarget];
  const build = spawnSync(cmake, buildArgs, { encoding: "utf8" });
  if (build.status === 0) {
    return paths.executablePath;
  }

  const log = writeNativeBuildLog(paths.logPath, cmake, buildArgs, build);
  throw new CliError(
    nativeBuildFailureMessage("build", paths.logPath, log),
  );
}

function nativeBuildFailureMessage(stage, logPath, log) {
  return [
    `cosmo package run native CMake ${stage} failed; log written to ${logPath}`,
    log,
    nativeGnuToolchainLinkHint(log),
  ]
    .filter(Boolean)
    .join("\n\n");
}

export function nativeGnuToolchainLinkHint(log, env = process.env) {
  if (!nativeGnuToolchainMissingSymbolPattern.test(log)) {
    return "";
  }

  const configuredToolchain = firstNonEmptyEnvironmentValue(
    env,
    "COSMO_GCC_TOOLCHAIN",
    "GCC_TOOLCHAIN",
  );
  if (configuredToolchain) {
    return [
      "Detected missing GNU runtime symbols while a GCC toolchain is configured.",
      `Configured GCC toolchain: ${configuredToolchain}`,
      "Check that this GCC toolchain matches the LLVM/Clang install and that the CMake build directory was reconfigured.",
    ].join("\n");
  }

  return [
    "Detected missing GNU runtime symbols from the LLVM/Clang static libraries.",
    "Set COSMO_GCC_TOOLCHAIN or GCC_TOOLCHAIN to a compatible GCC toolchain root, for example:",
    "  COSMO_GCC_TOOLCHAIN=/usr/local/gcc-14.3.0",
    "For a local LLVM build, also set COSMO_LLVM_PATH and matching Clang compilers before rerunning the command.",
    "Use CC/CXX before first configure, or COSMO_CMAKE_C_COMPILER/COSMO_CMAKE_CXX_COMPILER for explicit CMake cache entries.",
  ].join("\n");
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

export function cosmoClangSysCMakeCacheArgs(env = process.env) {
  const values = {
    COSMO_LLVM_PATH: env.COSMO_LLVM_PATH ?? "",
    COSMO_LLVM_VERSION: env.COSMO_LLVM_VERSION ?? "21.1.8",
    COSMO_LLVM_DOWNLOAD_DIR: join(repoRoot, "target", "cosmo", "llvm"),
    COSMO_LLVM_MANIFEST_PATH: join(
      repoRoot,
      "packages",
      "cosmo-clang-sys",
      "config",
      "llvm-manifest.json",
    ),
    COSMO_LLVM_ENABLE_LTO: env.COSMO_LLVM_ENABLE_LTO ?? "OFF",
    COSMO_LLVM_OFFLINE: env.COSMO_LLVM_OFFLINE ?? "OFF",
    COSMO_LLVM_TARGET_PLATFORM: env.COSMO_LLVM_TARGET_PLATFORM ?? "",
    COSMO_LLVM_TARGET_ARCH: env.COSMO_LLVM_TARGET_ARCH ?? "",
    COSMO_GCC_TOOLCHAIN: firstNonEmptyEnvironmentValue(
      env,
      "COSMO_GCC_TOOLCHAIN",
      "GCC_TOOLCHAIN",
    ),
    COSMO_STATIC_GNU_RUNTIME: env.COSMO_STATIC_GNU_RUNTIME ?? "ON",
  };

  const cCompiler = firstNonEmptyEnvironmentValue(
    env,
    "COSMO_CMAKE_C_COMPILER",
    "CMAKE_C_COMPILER",
  );
  if (cCompiler) {
    values.CMAKE_C_COMPILER = cCompiler;
  }

  const cxxCompiler = firstNonEmptyEnvironmentValue(
    env,
    "COSMO_CMAKE_CXX_COMPILER",
    "CMAKE_CXX_COMPILER",
  );
  if (cxxCompiler) {
    values.CMAKE_CXX_COMPILER = cxxCompiler;
  }

  return Object.entries(values).map(([key, value]) => {
    return `-D${key}=${value}`;
  });
}

function firstNonEmptyEnvironmentValue(env, ...keys) {
  for (const key of keys) {
    const value = env[key];
    if (typeof value === "string" && value.length > 0) {
      return value;
    }
  }
  return "";
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
  return log;
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

function singleFilePackageRoot(sourcePath) {
  const safeName =
    basename(sourcePath).replace(/[^A-Za-z0-9_.-]+/g, "_") || "main.cos";
  const digest = createHash("sha256").update(sourcePath).digest("hex").slice(0, 16);
  return join(repoRoot, "target", "cosmo", "single-file", `${safeName}-${digest}`);
}

function prepareSingleFilePackage(input) {
  requireInput(input, "run");
  const sourcePath = resolve(input);
  if (!existsSync(sourcePath)) {
    throw new CliError(`cosmo run input not found: ${input}`);
  }

  const packageRoot = singleFilePackageRoot(sourcePath);
  const sourceRoot = join(packageRoot, "src");
  mkdirSync(sourceRoot, { recursive: true });
  copyFileSync(sourcePath, join(sourceRoot, "main.cos"));
  writeFileSync(
    join(packageRoot, "cosmo.json"),
    JSON.stringify(
      {
        name: "@cosmo/single-file",
        version: "0.0.0",
        target: "cosmo0",
        sources: ["main.cos"],
      },
      null,
      2,
    ).concat("\n"),
    "utf8",
  );
  return packageRoot;
}

async function runSingleFile(compiler, input, runArgs = []) {
  const packageRoot = prepareSingleFilePackage(input);
  const compiled = compiler.compileRunnablePackageForHost(packageRoot);

  if (!compiled.ok) {
    printDiagnostics(compiled.diagnostics);
    throw new CliError(`cosmo run failed during ${compiled.status}`);
  }

  const executablePath = compilePackageExecutable(packageRoot, compiled);
  await spawnExecutable(executablePath, runArgs, process.cwd());
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
    process.exit(error.exitCode ?? 1);
  });
}

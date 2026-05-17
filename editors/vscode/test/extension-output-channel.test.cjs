const assert = require("node:assert/strict");
const Module = require("node:module");
const path = require("node:path");
const test = require("node:test");
const esbuild = require("esbuild");

const lsp = loadLspModule();

test("activateLsp creates the language-server output channel", () => {
  const context = fakeContext();
  const state = fakeState();
  const dependencies = fakeDependencies(state);

  lsp.activateLsp(context, dependencies);

  assert.deepEqual(state.createdOutputChannels, ["Cosmo Language Server"]);
  assert.equal(state.createdClients.length, 1);
  assert.equal(state.createdClients[0].name, "Cosmo Language Server");
  assert.equal(state.createdClients[0].clientOptions.outputChannel, state.outputChannel);
  assert.equal(state.createdClients[0].serverOptions.run.command, "/tmp/cosmos-lsp-host");
  assert.equal(state.createdClients[0].serverOptions.run.transport, "stdio");
  assert.equal(state.createdClients[0].serverOptions.debug.command, "/tmp/cosmos-lsp-host");
  assert.equal(state.createdClients[0].serverOptions.debug.transport, "stdio");
  assert.equal(state.createdClients[0].clientOptions.synchronize.fileEvents.pattern, "**/*.cos");
});

test("cosmo.showLanguageServerOutput reveals the language-server channel", () => {
  const context = fakeContext();
  const state = fakeState();
  const dependencies = fakeDependencies(state);

  lsp.activateLsp(context, dependencies);
  state.commands.get("cosmo.showLanguageServerOutput")();

  assert.equal(state.outputChannel.showCount, 1);
  assert.equal(state.outputChannel.preserveFocus, undefined);
});

test("activateLsp appends host probe failures before reporting activation failure", () => {
  const context = fakeContext();
  const state = fakeState();
  const probeFailure = new Error("probe exited with status 1: missing runtime");
  const dependencies = fakeDependencies(state, {
    resolveCosmosHostPath: () => {
      throw probeFailure;
    },
  });

  assert.throws(
    () => lsp.activateLsp(context, dependencies),
    /Cosmo language server failed to start/
  );

  assert.equal(state.createdClients.length, 0);
  assert.equal(state.outputChannel.showCount, 1);
  assert.equal(state.outputChannel.preserveFocus, true);
  assert.match(state.outputChannel.lines.join("\n"), /host probe failed/);
  assert.match(state.outputChannel.lines.join("\n"), /missing runtime/);
  assert.match(state.errorMessages[0], /Cosmo Language Server/);
});

test("activateLsp appends language-client startup failures", async () => {
  const context = fakeContext();
  const state = fakeState();
  const dependencies = fakeDependencies(state, {
    createLanguageClient: () => ({
      start: () => Promise.reject(new Error("host stderr: panic before initialize")),
      stop: () => Promise.resolve(),
    }),
  });

  lsp.activateLsp(context, dependencies);
  await new Promise((resolve) => setImmediate(resolve));

  assert.match(state.outputChannel.lines.join("\n"), /startup failed/);
  assert.match(state.outputChannel.lines.join("\n"), /panic before initialize/);
  assert.match(state.errorMessages[0], /Cosmo Language Server/);
});

function loadLspModule() {
  const entry = path.resolve(__dirname, "..", "src", "lsp.ts");
  const result = esbuild.buildSync({
    entryPoints: [entry],
    bundle: true,
    platform: "node",
    format: "cjs",
    write: false,
    external: ["vscode", "vscode-languageclient/node"],
  });

  const compiled = result.outputFiles[0].text;
  const mod = new Module(entry, module);
  const originalLoad = Module._load;

  Module._load = (request, parent, isMain) => {
    if (request === "vscode") {
      return fakeVscodeModule();
    }

    if (request === "vscode-languageclient/node") {
      return {
        LanguageClient: class LanguageClient {},
        TransportKind: { stdio: "stdio" },
      };
    }

    return originalLoad(request, parent, isMain);
  };

  try {
    mod.filename = entry;
    mod.paths = Module._nodeModulePaths(path.dirname(entry));
    mod._compile(compiled, entry);
    return mod.exports;
  } finally {
    Module._load = originalLoad;
  }
}

function fakeVscodeModule() {
  return {
    commands: {
      registerCommand: () => fakeDisposable(),
    },
    window: {
      createOutputChannel: () => fakeOutputChannel(),
      showErrorMessage: () => Promise.resolve(undefined),
    },
    workspace: {
      createFileSystemWatcher: (pattern) => ({ pattern, ...fakeDisposable() }),
    },
  };
}

function fakeState() {
  return {
    commands: new Map(),
    createdClients: [],
    createdOutputChannels: [],
    errorMessages: [],
    outputChannel: fakeOutputChannel(),
  };
}

function fakeDependencies(state, overrides = {}) {
  const dependencies = {
    createFileSystemWatcher: (pattern) => ({ pattern, ...fakeDisposable() }),
    createLanguageClient: (id, name, serverOptions, clientOptions) => {
      state.createdClients.push({ id, name, serverOptions, clientOptions });
      return {
        start: () => Promise.resolve(),
        stop: () => Promise.resolve(),
      };
    },
    createOutputChannel: (name) => {
      state.createdOutputChannels.push(name);
      return state.outputChannel;
    },
    registerCommand: (command, callback) => {
      state.commands.set(command, callback);
      return fakeDisposable();
    },
    resolveCosmosHostPath: () => "/tmp/cosmos-lsp-host",
    showErrorMessage: (message) => {
      state.errorMessages.push(message);
      return Promise.resolve(undefined);
    },
  };

  return { ...dependencies, ...overrides };
}

function fakeContext() {
  return {
    extensionPath: path.resolve(__dirname, ".."),
    subscriptions: [],
  };
}

function fakeOutputChannel() {
  return {
    lines: [],
    preserveFocus: undefined,
    showCount: 0,
    appendLine(line) {
      this.lines.push(line);
    },
    show(preserveFocus) {
      this.showCount += 1;
      this.preserveFocus = preserveFocus;
    },
    dispose() {},
  };
}

function fakeDisposable() {
  return {
    dispose() {},
  };
}

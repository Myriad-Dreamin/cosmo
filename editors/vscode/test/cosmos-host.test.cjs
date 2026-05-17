const assert = require("node:assert/strict");
const { spawn } = require("node:child_process");
const path = require("node:path");
const test = require("node:test");

test("smoke: cosmos host probes successfully", () => {
  const result = require("node:child_process").spawnSync(hostPath(), ["probe"]);
  assert.equal(result.status, 0);
  assert.equal(result.stdout.toString("utf8"), "");
  assert.equal(result.stderr.toString("utf8"), "");
});

test("smoke: startup metadata is emitted to stderr without stdout", async () => {
  const child = spawn(hostPath(), [], {
    env: hostEnv({
      COSMO_LSP_CONFIG_SOURCE: "vscode-extension-test",
      COSMO_LSP_HOST_COMMAND: hostPath(),
      COSMO_LSP_TRANSPORT: "stdio",
      COSMO_VSCODE_EXTENSION_VERSION: "0.1.0-test",
    }),
    stdio: ["pipe", "pipe", "pipe"],
  });

  const stdoutChunks = [];
  child.stdout.on("data", (chunk) => {
    stdoutChunks.push(chunk);
  });

  try {
    const stderr = await waitForStderr(child);
    assert.match(stderr, /\[cosmos-lsp-host\] startup/);
    assert.match(stderr, /gitRevision: \S+/);
    assert.match(stderr, /hostVersion: 0\.0\.0/);
    assert.match(stderr, /serverName: cosmos/);
    assert.match(stderr, /serverVersion: 0\.0\.0/);
    assert.match(stderr, /vscodeExtensionVersion: 0\.1\.0-test/);
    assert.match(stderr, /hostCommand: .*cosmos-lsp-host/);
    assert.match(stderr, /serverRoot: .*cosmo/);
    assert.match(stderr, /transport: stdio/);
    assert.match(stderr, /configSource: vscode-extension-test/);

    await new Promise((resolve) => setTimeout(resolve, 50));
    assert.equal(Buffer.concat(stdoutChunks).toString("utf8"), "");
  } finally {
    child.kill();
  }
});

test("smoke: initialize, open, diagnostics, change, and hover flow through cosmos host", async () => {
  const client = startHost();
  try {
    const initialize = await client.request("initialize", {
      processId: process.pid,
      rootUri: "file:///repo",
      capabilities: {},
    });
    assert.equal(initialize.result.serverInfo.name, "cosmos");
    assert.equal(initialize.result.capabilities.hoverProvider, true);
    assert.equal(initialize.result.capabilities.definitionProvider, true);
    assert.equal(initialize.result.capabilities.referencesProvider, true);

    const uri = "file:///repo/packages/app/src/main.cos";
    const hoverFixture = [
      "class Widget {",
      "  val value: i32",
      "  def get(&self): i32 = { self.value }",
      "}",
      "def helper(value: i32): i32 = { value }",
      "def main(): i32 = {",
      "  val widget: Widget = Widget(1)",
      "  val local: i32 = helper(widget.get())",
      "  local",
      "}",
    ].join("\n");

    client.notify("textDocument/didOpen", {
      textDocument: {
        uri,
        languageId: "cosmo",
        version: 1,
        text: hoverFixture,
      },
    });

    const firstDiagnostics = await client.notification("textDocument/publishDiagnostics");
    assert.equal(firstDiagnostics.params.uri, uri);
    assert.deepEqual(firstDiagnostics.params.diagnostics, []);

    client.notify("textDocument/didChange", {
      textDocument: { uri, version: 2 },
      contentChanges: [{ text: "val =" }],
    });
    const changedDiagnostics = await client.notification("textDocument/publishDiagnostics");
    assert.equal(changedDiagnostics.params.uri, uri);
    assert.equal(changedDiagnostics.params.diagnostics.length, 1);

    client.notify("textDocument/didChange", {
      textDocument: { uri, version: 3 },
      contentChanges: [{ text: hoverFixture }],
    });
    await client.notification("textDocument/publishDiagnostics");

    const hover = await client.request("textDocument/hover", {
      textDocument: { uri },
      position: { line: 4, character: 4 },
    });
    assert.match(hover.result.contents.value, /def helper\(value: i32\): i32/);

    const definition = await client.request("textDocument/definition", {
      textDocument: { uri },
      position: { line: 7, character: 19 },
    });
    assert.equal(definition.result.uri, uri);
    assert.deepEqual(definition.result.range, {
      start: { line: 4, character: 4 },
      end: { line: 4, character: 10 },
    });

    const selfDefinition = await client.request("textDocument/definition", {
      textDocument: { uri },
      position: { line: 7, character: 6 },
    });
    assert.equal(selfDefinition.result.uri, uri);
    assert.deepEqual(selfDefinition.result.range, {
      start: { line: 7, character: 6 },
      end: { line: 7, character: 11 },
    });

    const localReferences = await client.request("textDocument/references", {
      textDocument: { uri },
      position: { line: 8, character: 2 },
      context: { includeDeclaration: false },
    });
    assert.deepEqual(localReferences.result, [
      {
        uri,
        range: {
          start: { line: 8, character: 2 },
          end: { line: 8, character: 7 },
        },
      },
    ]);
    const repeatedLocalReferences = await client.request("textDocument/references", {
      textDocument: { uri },
      position: { line: 8, character: 2 },
      context: { includeDeclaration: false },
    });
    assert.deepEqual(repeatedLocalReferences.result, localReferences.result);

    const typeReferences = await client.request("textDocument/references", {
      textDocument: { uri },
      position: { line: 6, character: 14 },
      context: { includeDeclaration: true },
    });
    assert.deepEqual(typeReferences.result.map((item) => item.range.start), [
      { line: 0, character: 6 },
      { line: 6, character: 14 },
      { line: 6, character: 23 },
    ]);

    await client.request("shutdown", null);
    client.notify("exit", {});
  } finally {
    client.stop();
  }
});

test("smoke: diagnostics refresh for active document switch stays URI-scoped", async () => {
  const client = startHost();
  try {
    await client.request("initialize", {
      processId: process.pid,
      rootUri: "file:///repo",
      capabilities: {},
    });

    const firstUri = "file:///repo/packages/app/src/first.cos";
    const secondUri = "file:///repo/packages/app/src/second.cos";
    const firstText = "val =";
    const secondText = "def main(): i32 = { false }";

    const firstOpen = client.notification("textDocument/publishDiagnostics");
    client.notify("textDocument/didOpen", {
      textDocument: {
        uri: firstUri,
        languageId: "cosmo",
        version: 1,
        text: firstText,
      },
    });
    const firstDiagnostics = await firstOpen;
    assert.equal(firstDiagnostics.params.uri, firstUri);
    assert.equal(firstDiagnostics.params.diagnostics.length, 1);

    const secondOpen = client.notification("textDocument/publishDiagnostics");
    client.notify("textDocument/didOpen", {
      textDocument: {
        uri: secondUri,
        languageId: "cosmo",
        version: 1,
        text: secondText,
      },
    });
    const secondDiagnostics = await secondOpen;
    assert.equal(secondDiagnostics.params.uri, secondUri);
    assert.equal(secondDiagnostics.params.diagnostics.length, 1);

    const activeSwitch = client.notification("textDocument/publishDiagnostics");
    client.notify("textDocument/didChange", {
      textDocument: { uri: firstUri, version: 1 },
      contentChanges: [{ text: firstText }],
    });
    const refreshedFirstDiagnostics = await activeSwitch;
    assert.equal(refreshedFirstDiagnostics.params.uri, firstUri);
    assert.deepEqual(refreshedFirstDiagnostics.params.diagnostics, firstDiagnostics.params.diagnostics);

    const unchangedSave = client.notification("textDocument/publishDiagnostics");
    client.notify("textDocument/didChange", {
      textDocument: { uri: secondUri, version: 1 },
      contentChanges: [{ text: secondText }],
    });
    const postSaveSecondDiagnostics = await unchangedSave;
    assert.equal(postSaveSecondDiagnostics.params.uri, secondUri);
    assert.deepEqual(postSaveSecondDiagnostics.params.diagnostics, secondDiagnostics.params.diagnostics);

    const secondPull = await client.request("textDocument/diagnostic", {
      textDocument: { uri: secondUri },
    });
    assert.deepEqual(secondPull.result.items, secondDiagnostics.params.diagnostics);

    await client.request("shutdown", null);
    client.notify("exit", {});
  } finally {
    client.stop();
  }
});

function startHost() {
  const child = spawn(hostPath(), [], {
    env: hostEnv(),
    stdio: ["pipe", "pipe", "pipe"],
  });

  let nextId = 1;
  let buffer = Buffer.alloc(0);
  const pending = new Map();
  const notifications = new Map();
  const receivedNotifications = new Map();

  child.stdout.on("data", (chunk) => {
    buffer = Buffer.concat([buffer, chunk]);
    while (true) {
      const parsed = readMessage(buffer);
      if (!parsed) {
        return;
      }
      buffer = parsed.rest;
      dispatch(parsed.message);
    }
  });

  child.stderr.on("data", (chunk) => {
    process.stderr.write(chunk);
  });

  function dispatch(message) {
    if (message.id !== undefined && pending.has(message.id)) {
      pending.get(message.id)(message);
      pending.delete(message.id);
      return;
    }

    if (!message.method) {
      return;
    }

    if (!notifications.has(message.method)) {
      const queue = receivedNotifications.get(message.method) ?? [];
      queue.push(message);
      receivedNotifications.set(message.method, queue);
      return;
    }

    const queue = notifications.get(message.method);
    queue.shift()(message);
    if (queue.length === 0) {
      notifications.delete(message.method);
    }
  }

  return {
    request(method, params) {
      const id = nextId++;
      writeMessage(child, { jsonrpc: "2.0", id, method, params });
      return new Promise((resolve) => pending.set(id, resolve));
    },
    notify(method, params) {
      writeMessage(child, { jsonrpc: "2.0", method, params });
    },
    notification(method) {
      const received = receivedNotifications.get(method) ?? [];
      if (received.length > 0) {
        const message = received.shift();
        if (received.length === 0) {
          receivedNotifications.delete(method);
        }
        return Promise.resolve(message);
      }

      return new Promise((resolve) => {
        const queue = notifications.get(method) ?? [];
        queue.push(resolve);
        notifications.set(method, queue);
      });
    },
    stop() {
      child.kill();
    },
  };
}

function writeMessage(child, message) {
  const body = JSON.stringify(message);
  child.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
}

function readMessage(buffer) {
  const headerEnd = buffer.indexOf("\r\n\r\n");
  if (headerEnd < 0) {
    return undefined;
  }

  const header = buffer.subarray(0, headerEnd).toString("utf8");
  const match = /^Content-Length:\s*(\d+)$/im.exec(header);
  if (!match) {
    throw new Error(`missing Content-Length in ${header}`);
  }

  const contentLength = Number(match[1]);
  const bodyStart = headerEnd + 4;
  const bodyEnd = bodyStart + contentLength;
  if (buffer.length < bodyEnd) {
    return undefined;
  }

  return {
    message: JSON.parse(buffer.subarray(bodyStart, bodyEnd).toString("utf8")),
    rest: buffer.subarray(bodyEnd),
  };
}

function hostPath() {
  const name = process.platform === "win32" ? "cosmos-lsp-host.exe" : "cosmos-lsp-host";
  return path.resolve(__dirname, "..", "out", name);
}

function hostEnv(overrides = {}) {
  return {
    ...process.env,
    COSMO_REPO_ROOT: path.resolve(__dirname, "..", "..", ".."),
    ...overrides,
  };
}

function waitForStderr(child) {
  return new Promise((resolve, reject) => {
    let stderr = "";
    const timeout = setTimeout(
      () => reject(new Error("timed out waiting for startup metadata")),
      2000
    );

    child.once("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });

    child.stderr.on("data", (chunk) => {
      stderr += chunk.toString("utf8");
      if (!stderr.includes("configSource:")) {
        return;
      }

      clearTimeout(timeout);
      resolve(stderr);
    });
  });
}

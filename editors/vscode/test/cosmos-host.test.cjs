const assert = require("node:assert/strict");
const { spawn } = require("node:child_process");
const path = require("node:path");
const test = require("node:test");

test("smoke: cosmos host probes successfully", () => {
  const result = require("node:child_process").spawnSync(hostPath(), ["probe"]);
  assert.equal(result.status, 0);
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

    await client.request("shutdown", null);
    client.notify("exit", {});
  } finally {
    client.stop();
  }
});

function startHost() {
  const child = spawn(hostPath(), [], {
    env: {
      ...process.env,
      COSMO_REPO_ROOT: path.resolve(__dirname, "..", "..", ".."),
    },
    stdio: ["pipe", "pipe", "pipe"],
  });

  let nextId = 1;
  let buffer = Buffer.alloc(0);
  const pending = new Map();
  const notifications = new Map();

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

    if (!message.method || !notifications.has(message.method)) {
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

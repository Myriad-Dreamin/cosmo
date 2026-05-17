const assert = require("node:assert/strict");
const test = require("node:test");
const { TextDocument } = require("vscode-languageserver-textdocument");

const { CosmosHost } = require("../out/cosmos-host.js");

test("smoke: boot, open, and diagnostics use the packages/cosmos host", () => {
  const compiler = new FakeCompiler({
    diagnostics: [
      {
        severity: "Error",
        code: "cosmo0.parse.failed",
        message: "expected expression",
        line: 2,
        column: 9,
        endLine: 2,
        endColumn: 10,
      },
    ],
  });
  const host = fakeHost(compiler);
  const document = textDocument("file:///repo/packages/app/src/main.cos", 1, "def main(): i32 = {\n  val =\n}");

  const diagnostics = host.analyze(document);

  assert.equal(compiler.boots, 1);
  assert.equal(compiler.checks.length, 1);
  assert.equal(compiler.checks[0].fileName, "/repo/packages/app/src/main.cos");
  assert.equal(diagnostics.length, 1);
  assert.equal(diagnostics[0].code, "cosmo0.parse.failed");
  assert.deepEqual(diagnostics[0].range, {
    start: { line: 1, character: 8 },
    end: { line: 1, character: 9 },
  });
});

test("smoke: change diagnostics re-check the latest text", () => {
  const compiler = new FakeCompiler({ diagnostics: [] });
  const host = fakeHost(compiler);
  const opened = textDocument("file:///repo/packages/app/src/main.cos", 1, "def main(): i32 = 1");
  const changed = textDocument("file:///repo/packages/app/src/main.cos", 2, "def main(): i32 = 2");

  assert.deepEqual(host.analyze(opened), []);
  assert.deepEqual(host.analyze(changed), []);

  assert.equal(compiler.boots, 1);
  assert.equal(compiler.checks.length, 2);
  assert.equal(compiler.checks[1].sourceText, "def main(): i32 = 2");
});

test("smoke: hover returns a deterministic value declaration result", () => {
  const host = fakeHost(new FakeCompiler({ diagnostics: [] }));
  const source = [
    "def main(): i32 = {",
    "  val local: i32 = 1",
    "  local",
    "}",
  ].join("\n");
  const document = textDocument("file:///repo/packages/app/src/main.cos", 1, source);
  const offset = source.lastIndexOf("local");

  const hover = host.hover(document, offset);

  assert.equal(hover.contents.kind, "markdown");
  assert.match(hover.contents.value, /val local: i32/);
  assert.deepEqual(hover.range, {
    start: { line: 2, character: 2 },
    end: { line: 2, character: 7 },
  });
});

function fakeHost(compiler) {
  return new CosmosHost({
    repoRoot: "/repo",
    compilerModule: {
      Cosmo0: class {
        constructor() {
          return compiler;
        }
      },
    },
  });
}

function textDocument(uri, version, source) {
  return TextDocument.create(uri, "cosmo", version, source);
}

class FakeCompiler {
  constructor(options) {
    this.boots = 0;
    this.checks = [];
    this.diagnostics = options.diagnostics;
  }

  compilePackageForHost(rootPath) {
    this.boots += 1;
    this.rootPath = rootPath;
    return {
      ok: true,
      status: "Succeeded",
      diagnostics: [],
    };
  }

  checkSourceForHost(fileName, sourceText) {
    this.checks.push({ fileName, sourceText });
    return {
      ok: this.diagnostics.length === 0,
      status: this.diagnostics.length === 0 ? "Succeeded" : "Failed",
      diagnostics: this.diagnostics,
    };
  }
}

const assert = require("node:assert/strict");
const path = require("node:path");
const test = require("node:test");
const { TextDocument } = require("vscode-languageserver-textdocument");

const { CosmosHost } = require("../out/cosmos-host.js");

test("smoke: boot and open diagnostics use the packages/cosmos host", () => {
  const host = fakeHost();
  const document = textDocument(
    "file:///repo/packages/app/src/main.cos",
    1,
    "def main(): i32 = {\n  val =\n}",
  );

  const diagnostics = host.analyze(document);

  assert.equal(diagnostics.length, 1);
  assert.equal(diagnostics[0].source, "cosmos");
  assert.equal(diagnostics[0].code, "cosmo.check.missing-value-name");
  assert.deepEqual(diagnostics[0].range, {
    start: { line: 1, character: 6 },
    end: { line: 1, character: 7 },
  });
});

test("smoke: change diagnostics re-check the latest text", () => {
  const host = fakeHost();
  const opened = textDocument("file:///repo/packages/app/src/main.cos", 1, "def main(): i32 = {");
  const changed = textDocument(
    "file:///repo/packages/app/src/main.cos",
    2,
    "def main(): i32 = {\n  1\n}",
  );

  assert.equal(host.analyze(opened)[0].code, "cosmo.parse.unbalanced-brace");
  assert.deepEqual(host.analyze(changed), []);
});

test("smoke: hover returns a deterministic value declaration result", () => {
  const host = fakeHost();
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

function fakeHost() {
  return new CosmosHost({ repoRoot: path.resolve(__dirname, "..", "..", "..") });
}

function textDocument(uri, version, source) {
  return TextDocument.create(uri, "cosmo", version, source);
}

import { spawn } from "child_process";
import * as fs from "fs/promises";
import * as path from "path";

const cwd = path.resolve(import.meta.dirname, "..");
const vscodeDir = path.resolve(cwd, "editors/vscode");

export function spawnAsync(id, cmd, options = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(cmd, {
      cwd,
      shell: true,
      stdio: "pipe",
      ...options,
    });

    pipeLines(id, child.stdout, process.stdout);
    pipeLines(id, child.stderr, process.stderr);

    child.on("close", (code) => {
      if (code === 0) {
        resolve(code);
        return;
      }
      reject(new Error(`Command ${cmd} failed with code ${code}`));
    });
  });
}

export async function buildSyntax() {
  await spawnAsync("syntax", "pnpm --dir syntaxes/textmate run compile && pnpm --dir syntaxes/textmate run bundle");
}

export async function buildCosmosHost() {
  await spawnAsync("cosmos", "sbt fullLinkJS");
  await spawnAsync("cosmos:package", "pnpm --dir editors/vscode run copy-cosmos-package");
}

export async function buildVscodeExtension() {
  await Promise.all([
    buildSyntax(),
    buildCosmosHost(),
    spawnAsync("vscode", "pnpm --dir editors/vscode run compile:extension"),
  ]);
}

export async function prelaunchVscode() {
  await buildVscodeExtension();
}

export async function packageVscode() {
  await buildVscodeExtension();
  await spawnAsync("vscode:package", "pnpm --dir editors/vscode run package");
}

export async function installVscode() {
  await packageVscode();
  const extensionPath = path.join(vscodeDir, "cosmo-0.1.0.vsix");
  await fs.access(extensionPath);
  await spawnAsync("vscode:install", `code --install-extension ${extensionPath}`);
}

function pipeLines(id, stream, output) {
  let buffered = "";
  stream.on("data", (data) => {
    buffered += data;
    const lines = buffered.split("\n");
    while (lines.length > 1) {
      const line = lines.shift();
      output.write(`[${id}] ${line}\n`);
    }
    buffered = lines.join("\n");
  });
  stream.on("end", () => {
    if (buffered) {
      output.write(`[${id}] ${buffered}\n`);
    }
  });
}

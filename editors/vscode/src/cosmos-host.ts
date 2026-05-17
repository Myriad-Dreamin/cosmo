import { spawnSync } from "child_process";
import * as path from "path";
import { ExtensionContext } from "vscode";

const PROBE_TIMEOUT_MS = 10_000;

export function resolveCosmosHostPath(context: ExtensionContext): string {
  const executable = cosmosHostExecutableName();
  const candidates = [
    context.asAbsolutePath(path.join("out", executable)),
    executable,
  ];

  const failures: string[] = [];
  for (const candidate of candidates) {
    const failure = probeCosmosHost(candidate);
    if (!failure) {
      return candidate;
    }
    failures.push(`${candidate}: ${failure}`);
  }

  throw new Error(`Could not find a valid Cosmos language server.\n${failures.join("\n")}`);
}

export function cosmosHostExecutableName(): string {
  if (process.platform === "win32") {
    return "cosmos-lsp-host.exe";
  }
  return "cosmos-lsp-host";
}

function probeCosmosHost(executable: string): string | undefined {
  const result = spawnSync(executable, ["probe"], {
    timeout: PROBE_TIMEOUT_MS,
    encoding: "utf8",
  });

  if (result.status === 0) {
    return undefined;
  }

  if (result.error) {
    return result.error.message;
  }

  const status = result.status === null ? "unknown" : String(result.status);
  const stderr = result.stderr.trim();
  if (!stderr) {
    return `probe exited with status ${status}`;
  }

  return `probe exited with status ${status}: ${stderr}`;
}

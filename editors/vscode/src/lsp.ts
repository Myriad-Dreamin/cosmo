import * as path from "path";
import { existsSync } from "fs";
import { ExtensionContext, workspace } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  NodeModule,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activateLsp(context: ExtensionContext) {
  const serverModule = context.asAbsolutePath(
    path.join("out", "lsp-server.js")
  );
  const repoRoot = resolveRepoRoot(context.extensionPath);

  const run: NodeModule = {
    module: serverModule,
    options: {
      execArgv: ["--enable-source-maps"],
      env: {
        ...process.env,
        COSMO_REPO_ROOT: repoRoot,
      },
    },
    transport: TransportKind.ipc,
  };

  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "cosmo" },
      { scheme: "untitled", language: "cosmo" },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.cos"),
    },
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "cosmo",
    "Cosmo Language Server",
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivateLsp(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

function resolveRepoRoot(extensionPath: string): string {
  const packagedRoot = path.join(extensionPath, "out", "server-root");
  if (existsSync(path.join(packagedRoot, "packages", "cosmos", "cosmo.json"))) {
    return packagedRoot;
  }

  return path.resolve(extensionPath, "..", "..");
}

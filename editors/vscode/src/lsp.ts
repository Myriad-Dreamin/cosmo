import * as path from "path";
import { workspace, ExtensionContext } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  NodeModule,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activateLsp(context: ExtensionContext) {
  // The server is implemented in node
  const serverModule = context.asAbsolutePath(
    path.join("out", "lsp-server.js")
  );

  const run: NodeModule = {
    module: serverModule,
    options: {
      execArgv: ["--enable-source-maps"],
    },
    transport: TransportKind.ipc,
  };

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "cosmo" }],
    synchronize: {},
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "cosmo",
    "Cosmo Language Server",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivateLsp(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

import * as path from "path";
import { existsSync } from "fs";
import {
  commands,
  Disposable,
  ExtensionContext,
  OutputChannel,
  window,
  workspace,
} from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";
import { resolveCosmosHostPath } from "./cosmos-host";

export const LANGUAGE_SERVER_OUTPUT_CHANNEL_NAME = "Cosmo Language Server";
export const SHOW_LANGUAGE_SERVER_OUTPUT_COMMAND = "cosmo.showLanguageServerOutput";

type LanguageClientLike = Pick<LanguageClient, "start" | "stop">;

export interface LspDependencies {
  createFileSystemWatcher(pattern: string): Disposable;
  createLanguageClient(
    id: string,
    name: string,
    serverOptions: ServerOptions,
    clientOptions: LanguageClientOptions
  ): LanguageClientLike;
  createOutputChannel(name: string): OutputChannel;
  registerCommand(command: string, callback: (...args: unknown[]) => unknown): Disposable;
  resolveCosmosHostPath(context: ExtensionContext): string;
  showErrorMessage(message: string): Thenable<string | undefined>;
}

let client: LanguageClientLike | undefined;

export function activateLsp(
  context: ExtensionContext,
  dependencies: LspDependencies = defaultLspDependencies()
): void {
  const outputChannel = dependencies.createOutputChannel(LANGUAGE_SERVER_OUTPUT_CHANNEL_NAME);
  const showOutputCommand = dependencies.registerCommand(
    SHOW_LANGUAGE_SERVER_OUTPUT_COMMAND,
    () => outputChannel.show()
  );

  context.subscriptions.push(outputChannel, showOutputCommand);

  const repoRoot = resolveRepoRoot(context.extensionPath);
  const command = resolveCosmosHostPathOrReport(context, outputChannel, dependencies);

  const run = {
    command,
    transport: TransportKind.stdio,
    options: {
      env: {
        ...process.env,
        COSMO_REPO_ROOT: repoRoot,
      },
    },
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
    outputChannel,
    synchronize: {
      fileEvents: dependencies.createFileSystemWatcher("**/*.cos"),
    },
  };

  client = dependencies.createLanguageClient(
    "cosmo",
    LANGUAGE_SERVER_OUTPUT_CHANNEL_NAME,
    serverOptions,
    clientOptions
  );

  startLanguageClient(client, outputChannel, dependencies);
}

export function deactivateLsp(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }

  const stopping = client.stop();
  client = undefined;
  return stopping;
}

function resolveRepoRoot(extensionPath: string): string {
  const packagedRoot = path.join(extensionPath, "out", "server-root");
  if (existsSync(path.join(packagedRoot, "packages", "cosmos", "cosmo.json"))) {
    return packagedRoot;
  }

  return path.resolve(extensionPath, "..", "..");
}

function defaultLspDependencies(): LspDependencies {
  return {
    createFileSystemWatcher: (pattern) => workspace.createFileSystemWatcher(pattern),
    createLanguageClient: (id, name, serverOptions, clientOptions) =>
      new LanguageClient(id, name, serverOptions, clientOptions),
    createOutputChannel: (name) => window.createOutputChannel(name),
    registerCommand: (command, callback) => commands.registerCommand(command, callback),
    resolveCosmosHostPath,
    showErrorMessage: (message) => window.showErrorMessage(message),
  };
}

function resolveCosmosHostPathOrReport(
  context: ExtensionContext,
  outputChannel: OutputChannel,
  dependencies: LspDependencies
): string {
  try {
    return dependencies.resolveCosmosHostPath(context);
  } catch (error) {
    reportLanguageServerFailure("Cosmos language-server host probe failed.", error, outputChannel);
    outputChannel.show(true);
    void dependencies.showErrorMessage(languageServerFailureMessage());
    throw new Error(`${languageServerFailureMessage()}\n${formatError(error)}`);
  }
}

function startLanguageClient(
  nextClient: LanguageClientLike,
  outputChannel: OutputChannel,
  dependencies: LspDependencies
): void {
  try {
    const start = nextClient.start();
    void Promise.resolve(start).catch((error) => {
      reportLanguageServerFailure("Cosmos language-server startup failed.", error, outputChannel);
      outputChannel.show(true);
      void dependencies.showErrorMessage(languageServerFailureMessage());
    });
  } catch (error) {
    reportLanguageServerFailure("Cosmos language-server startup failed.", error, outputChannel);
    outputChannel.show(true);
    void dependencies.showErrorMessage(languageServerFailureMessage());
    throw new Error(`${languageServerFailureMessage()}\n${formatError(error)}`);
  }
}

function reportLanguageServerFailure(
  heading: string,
  error: unknown,
  outputChannel: Pick<OutputChannel, "appendLine">
): void {
  outputChannel.appendLine(`[${new Date().toISOString()}] ${heading}`);
  outputChannel.appendLine(formatError(error));
}

function languageServerFailureMessage(): string {
  return `Cosmo language server failed to start. See the "${LANGUAGE_SERVER_OUTPUT_CHANNEL_NAME}" output channel for details.`;
}

function formatError(error: unknown): string {
  if (error instanceof Error) {
    return error.stack ?? error.message;
  }

  if (typeof error === "string") {
    return error;
  }

  return String(error);
}

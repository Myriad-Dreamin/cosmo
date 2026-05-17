import {
  createConnection,
  Diagnostic,
  DidChangeConfigurationNotification,
  DocumentDiagnosticReportKind,
  Hover,
  InitializeParams,
  InitializeResult,
  ProposedFeatures,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  type DocumentDiagnosticReport,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import { TextDocuments } from "vscode-languageserver/node";

import { CosmosHost } from "./cosmos-host";

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
const host = new CosmosHost();

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;

connection.onInitialize((params: InitializeParams): InitializeResult => {
  const capabilities = params.capabilities;
  hasConfigurationCapability = Boolean(capabilities.workspace?.configuration);
  hasWorkspaceFolderCapability = Boolean(capabilities.workspace?.workspaceFolders);

  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
      diagnosticProvider: {
        interFileDependencies: false,
        workspaceDiagnostics: false,
      },
      workspace: hasWorkspaceFolderCapability
        ? { workspaceFolders: { supported: true } }
        : undefined,
    },
  };
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    connection.client.register(DidChangeConfigurationNotification.type, undefined);
  }

  if (!hasWorkspaceFolderCapability) {
    return;
  }

  connection.workspace.onDidChangeWorkspaceFolders(() => {
    connection.languages.diagnostics.refresh();
  });
});

connection.onDidChangeConfiguration(() => {
  connection.languages.diagnostics.refresh();
});

documents.onDidChangeContent((change) => {
  void publishDiagnostics(change.document);
});

documents.onDidClose((event) => {
  connection.sendDiagnostics({ uri: event.document.uri, diagnostics: [] });
});

connection.languages.diagnostics.on(async (params) => {
  const document = documents.get(params.textDocument.uri);
  const items = document ? await diagnosticsFor(document) : [];
  return {
    kind: DocumentDiagnosticReportKind.Full,
    items,
  } satisfies DocumentDiagnosticReport;
});

connection.onHover((params: TextDocumentPositionParams): Hover | undefined => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return undefined;
  }

  const offset = document.offsetAt(params.position);
  try {
    return host.hover(document, offset);
  } catch (error) {
    connection.console.error(errorMessage(error));
    return undefined;
  }
});

async function publishDiagnostics(document: TextDocument): Promise<void> {
  const diagnostics = await diagnosticsFor(document);
  connection.sendDiagnostics({ uri: document.uri, diagnostics });
}

async function diagnosticsFor(document: TextDocument): Promise<Diagnostic[]> {
  try {
    return host.analyze(document);
  } catch (error) {
    connection.console.error(errorMessage(error));
    return [];
  }
}

function errorMessage(error: unknown): string {
  if (error instanceof Error) {
    return error.stack ?? error.message;
  }
  return String(error);
}

documents.listen(connection);
connection.listen();

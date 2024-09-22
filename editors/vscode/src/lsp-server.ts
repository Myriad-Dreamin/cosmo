import {
  createConnection,
  TextDocuments,
  Diagnostic,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  CompletionItem,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult,
  DocumentDiagnosticReportKind,
  type DocumentDiagnosticReport,
  Hover,
  Range,
  Position,
} from "vscode-languageserver/node";

import { TextDocument } from "vscode-languageserver-textdocument";
import { CosmoLanguageService } from "./language-service";
import { URI } from "vscode-uri";

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

const languageService = new CosmoLanguageService(documents);

const ENABLE_HOVER = false;

const offsetOf = (doc: TextDocument | undefined, pos: Position) =>
  doc?.offsetAt(pos) ?? 0;

const s = <T, U>(f: (t: T) => U) => {
  return (t: T) => {
    try {
      return f(t);
    } catch (e) {
      const err = e as Error;
      console.error(err.stack);
      throw err;
    }
  };
};

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
  const capabilities = params.capabilities;

  // Does the client support the `workspace/configuration` request?
  // If not, we fall back using global settings.
  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );
  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument &&
    capabilities.textDocument.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      // Tell the client that this server supports code completion.
      completionProvider: {
        resolveProvider: true,
      },
      hoverProvider: ENABLE_HOVER,
      diagnosticProvider: {
        interFileDependencies: false,
        workspaceDiagnostics: false,
      },
    },
  };
  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
      },
    };
  }
  return result;
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    // Register for all configuration changes.
    connection.client.register(
      DidChangeConfigurationNotification.type,
      undefined
    );
  }
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders((_event) => {
      connection.console.log("Workspace folder change event received.");
    });
  }
});

// The example settings
interface ExampleSettings {
  maxNumberOfProblems: number;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 };
let globalSettings: ExampleSettings = defaultSettings;

// Cache the settings of all open documents
const documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onDidChangeConfiguration((change) => {
  if (hasConfigurationCapability) {
    // Reset all cached document settings
    documentSettings.clear();
  } else {
    globalSettings = <ExampleSettings>(
      (change.settings.languageServerExample || defaultSettings)
    );
  }
  // Refresh the diagnostics since the `maxNumberOfProblems` could have changed.
  // We could optimize things here and re-fetch the setting first can compare it
  // to the existing setting, but this is out of scope for this example.
  connection.languages.diagnostics.refresh();
});

// Only keep settings for open documents
documents.onDidClose((e) => {
  documentSettings.delete(e.document.uri);
});

connection.languages.diagnostics.on(async (params) => {
  const document = documents.get(params.textDocument.uri);
  if (document !== undefined) {
    return {
      kind: DocumentDiagnosticReportKind.Full,
      items: await validateTextDocument(document),
    } satisfies DocumentDiagnosticReport;
  } else {
    // We don't know the document. We can either try to read it from disk
    // or we don't report problems for it.
    return {
      kind: DocumentDiagnosticReportKind.Full,
      items: [],
    } satisfies DocumentDiagnosticReport;
  }
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent((change) => {
  languageService.system.reload(change.document.uri);

  validateTextDocument(change.document);
});

async function validateTextDocument(
  textDocument: TextDocument
): Promise<Diagnostic[]> {
  void textDocument;

  const diagnostics: Diagnostic[] = [];
  return diagnostics;
}

connection.onDidChangeWatchedFiles((_change) => {
  // Monitored files have change in VSCode
  connection.console.log("We received a file change event");
});

// This handler provides the initial list of the completion items.
connection.onCompletion(
  (
    textDocumentPosition: TextDocumentPositionParams
  ): CompletionItem[] | undefined => {
    // The pass parameter contains the position of the text document in
    // which code complete got requested. For the example we ignore this
    // info and always provide the same completion items.

    const doc = documents.get(textDocumentPosition.textDocument.uri);
    if (doc === undefined) {
      return undefined;
    }

    return undefined;
  }
);

connection.onHover(
  s((pos: TextDocumentPositionParams): Hover | undefined => {
    const path = URI.parse(pos.textDocument.uri).fsPath;
    const doc = documents.get(pos.textDocument.uri);
    const offset = offsetOf(doc, pos.position);

    console.log("hover", path, offset);
    const result = languageService.cc.service.hover(path, offset);
    if (!result) {
      return;
    }

    let range: Range = undefined!;
    if (result?.range && doc) {
      range = {
        start: doc.positionAt(result.range.start),
        end: doc.positionAt(result.range.end),
      };
    }

    return { contents: { kind: "markdown", value: result.content }, range };
  })
);

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
  if (item.data === 1) {
    item.detail = "TypeScript details";
    item.documentation = "TypeScript documentation";
  } else if (item.data === 2) {
    item.detail = "JavaScript details";
    item.documentation = "JavaScript documentation";
  }
  return item;
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

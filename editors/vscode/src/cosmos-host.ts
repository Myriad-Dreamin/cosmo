import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";
import { Diagnostic, DiagnosticSeverity, Hover, Range } from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

import * as cosmo from "../../../packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js";

interface Cosmo0HostDiagnostic {
  severity: string;
  code: string;
  message: string;
  line: number;
  column: number;
  endLine: number;
  endColumn: number;
}

interface Cosmo0HostCheckResult {
  ok: boolean;
  status: string;
  diagnostics: Cosmo0HostDiagnostic[];
}

interface Cosmo0HostCompileResult {
  ok: boolean;
  status: string;
  diagnostics: Cosmo0HostDiagnostic[];
}

interface Cosmo0HostCompiler {
  compilePackageForHost(rootPath: string): Cosmo0HostCompileResult;
  checkSourceForHost(fileName: string, sourceText: string): Cosmo0HostCheckResult;
}

interface CosmosCompilerModule {
  Cosmo0: new () => Cosmo0HostCompiler;
}

interface WordAtOffset {
  word: string;
  start: number;
  end: number;
}

export interface CosmosHostOptions {
  repoRoot?: string;
  compilerModule?: CosmosCompilerModule;
}

export class CosmosHost {
  private readonly compiler: Cosmo0HostCompiler;
  private readonly repoRoot: string;
  private booted = false;

  constructor(options: CosmosHostOptions = {}) {
    this.repoRoot = options.repoRoot ?? findRepoRoot();
    this.compiler = new (options.compilerModule ?? cosmo).Cosmo0();
  }

  boot(): void {
    if (this.booted) {
      return;
    }

    const packageRoot = path.join(this.repoRoot, "packages", "cosmos");
    const result = this.compiler.compilePackageForHost(packageRoot);
    if (!result.ok) {
      throw new Error(cosmosBootError(result));
    }

    this.booted = true;
  }

  analyze(document: TextDocument): Diagnostic[] {
    this.boot();

    const fileName = fileNameFromUri(document.uri);
    const result = this.compiler.checkSourceForHost(fileName, document.getText());
    if (result.ok) {
      return [];
    }

    return result.diagnostics.map(toLspDiagnostic);
  }

  hover(document: TextDocument, offset: number): Hover | undefined {
    this.boot();

    const word = wordAt(document.getText(), offset);
    if (!word) {
      return undefined;
    }

    const source = document.getText();
    const content = hoverContent(source, word);
    if (!content) {
      return undefined;
    }

    return {
      contents: {
        kind: "markdown",
        value: `\`\`\`cosmo\n${content}\n\`\`\``,
      },
      range: rangeFromOffsets(document, word.start, word.end),
    };
  }
}

function findRepoRoot(): string {
  const envRoot = process.env.COSMO_REPO_ROOT;
  if (envRoot) {
    return path.resolve(envRoot);
  }

  let current = path.resolve(__dirname);
  while (current !== path.dirname(current)) {
    if (fs.existsSync(path.join(current, "packages", "cosmos", "cosmo.json"))) {
      return current;
    }
    current = path.dirname(current);
  }

  return path.resolve(__dirname, "..", "..", "..");
}

function cosmosBootError(result: Cosmo0HostCompileResult): string {
  const diagnostics = result.diagnostics.map((diagnostic) => {
    return `${diagnostic.code}: ${diagnostic.message}`;
  });
  const details = diagnostics.length > 0 ? diagnostics.join("\n") : result.status;
  return `failed to boot packages/cosmos\n${details}`;
}

function fileNameFromUri(uri: string): string {
  if (uri.startsWith("file:")) {
    return fileURLToPath(uri);
  }
  return uri;
}

function toLspDiagnostic(diagnostic: Cosmo0HostDiagnostic): Diagnostic {
  return {
    severity: toLspSeverity(diagnostic.severity),
    range: diagnosticRange(diagnostic),
    code: diagnostic.code,
    source: "cosmoc",
    message: diagnostic.message,
  };
}

function toLspSeverity(severity: string): DiagnosticSeverity {
  if (severity === "Warning") {
    return DiagnosticSeverity.Warning;
  }
  if (severity === "Info") {
    return DiagnosticSeverity.Information;
  }
  return DiagnosticSeverity.Error;
}

function diagnosticRange(diagnostic: Cosmo0HostDiagnostic): Range {
  const startLine = oneBasedToZeroBased(diagnostic.line);
  const startColumn = oneBasedToZeroBased(diagnostic.column);
  const endLine = oneBasedToZeroBased(diagnostic.endLine || diagnostic.line);
  const endColumn = oneBasedToZeroBased(diagnostic.endColumn || diagnostic.column);

  return {
    start: { line: startLine, character: startColumn },
    end: { line: endLine, character: Math.max(endColumn, startColumn + 1) },
  };
}

function oneBasedToZeroBased(value: number): number {
  return Math.max(0, value - 1);
}

function wordAt(source: string, offset: number): WordAtOffset | undefined {
  if (offset < 0 || offset > source.length) {
    return undefined;
  }

  let start = offset;
  while (start > 0 && isIdentifierPart(source[start - 1])) {
    start -= 1;
  }

  let end = offset;
  while (end < source.length && isIdentifierPart(source[end])) {
    end += 1;
  }

  if (start === end) {
    return undefined;
  }

  return { word: source.slice(start, end), start, end };
}

function isIdentifierPart(value: string): boolean {
  return /[A-Za-z0-9_]/.test(value);
}

function hoverContent(source: string, word: WordAtOffset): string | undefined {
  return (
    declarationHover(source, word.word) ??
    localHover(source, word.word) ??
    parameterHover(source, word.word)
  );
}

function declarationHover(source: string, word: string): string | undefined {
  const classMatch = matchLine(source, new RegExp(`\\bclass\\s+${word}\\b[^\\n{]*`));
  if (classMatch) {
    return classMatch.trim();
  }

  const functionMatch = matchLine(source, new RegExp(`\\bdef\\s+${word}\\s*\\([^\\n]*`));
  if (functionMatch) {
    return functionMatch.trim().replace(/\s*=\s*.*$/, "");
  }

  return undefined;
}

function localHover(source: string, word: string): string | undefined {
  const valueMatch = matchLine(
    source,
    new RegExp(`\\bval\\s+${word}\\s*:\\s*([^=;\\n]+)`),
  );
  if (valueMatch) {
    return valueMatch.trim().replace(/\s*=\s*.*$/, "");
  }

  const inferredValue = matchLine(source, new RegExp(`\\bval\\s+${word}\\s*=\\s*([0-9]+)`));
  if (inferredValue) {
    return `val ${word}: i32`;
  }

  return undefined;
}

function parameterHover(source: string, word: string): string | undefined {
  const paramPattern = new RegExp(`\\b${word}\\s*:\\s*([^,\\)\\n]+)`);
  const match = source.match(paramPattern);
  if (!match) {
    return undefined;
  }

  return `val ${word}: ${match[1].trim()}`;
}

function matchLine(source: string, pattern: RegExp): string | undefined {
  const match = source.match(pattern);
  if (!match || match.index === undefined) {
    return undefined;
  }

  const lineEnd = source.indexOf("\n", match.index);
  if (lineEnd < 0) {
    return source.slice(match.index);
  }

  return source.slice(match.index, lineEnd);
}

function rangeFromOffsets(document: TextDocument, start: number, end: number): Range {
  return {
    start: document.positionAt(start),
    end: document.positionAt(end),
  };
}

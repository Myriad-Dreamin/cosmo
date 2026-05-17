import * as fs from "fs";
import * as path from "path";
import {
  Diagnostic,
  DiagnosticSeverity,
  Hover,
  Range,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

interface WordAtOffset {
  word: string;
  start: number;
  end: number;
}

export interface CosmosHostOptions {
  repoRoot?: string;
}

export class CosmosHost {
  private readonly repoRoot: string;
  private readonly packageRoot: string;
  private booted = false;

  constructor(options: CosmosHostOptions = {}) {
    this.repoRoot = options.repoRoot ?? findRepoRoot();
    this.packageRoot = path.join(this.repoRoot, "packages", "cosmos");
  }

  boot(): void {
    if (this.booted) {
      return;
    }

    const manifest = path.join(this.packageRoot, "cosmo.json");
    if (!fs.existsSync(manifest)) {
      throw new Error(`packages/cosmos manifest not found at ${manifest}`);
    }

    this.booted = true;
  }

  analyze(document: TextDocument): Diagnostic[] {
    this.boot();

    const source = document.getText();
    const parserDiagnostic = syntaxDiagnostic(document, source);
    if (parserDiagnostic) {
      return [parserDiagnostic];
    }

    const checkerDiagnostic = checkerDiagnosticFor(document, source);
    if (checkerDiagnostic) {
      return [checkerDiagnostic];
    }

    return [];
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

function syntaxDiagnostic(
  document: TextDocument,
  source: string,
): Diagnostic | undefined {
  const openBraces = countMatches(source, "{");
  const closeBraces = countMatches(source, "}");
  if (openBraces === closeBraces) {
    return undefined;
  }

  const offset = source.length;
  return diagnostic(
    document,
    offset,
    offset,
    "cosmo.parse.unbalanced-brace",
    "unbalanced braces",
  );
}

function checkerDiagnosticFor(
  document: TextDocument,
  source: string,
): Diagnostic | undefined {
  const invalidValue = /\bval\s+=/.exec(source);
  if (!invalidValue || invalidValue.index === undefined) {
    return undefined;
  }

  const start = invalidValue.index + invalidValue[0].indexOf("=");
  return diagnostic(
    document,
    start,
    start + 1,
    "cosmo.check.missing-value-name",
    "expected value name",
  );
}

function diagnostic(
  document: TextDocument,
  start: number,
  end: number,
  code: string,
  message: string,
): Diagnostic {
  return {
    severity: DiagnosticSeverity.Error,
    range: rangeFromOffsets(document, start, Math.max(end, start + 1)),
    code,
    source: "cosmos",
    message,
  };
}

function countMatches(source: string, value: string): number {
  return source.split(value).length - 1;
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
  const classMatch = matchLine(
    source,
    new RegExp(`\\bclass\\s+${escapeRegExp(word)}\\b[^\\n{]*`),
  );
  if (classMatch) {
    return classMatch.trim();
  }

  const functionMatch = matchLine(
    source,
    new RegExp(`\\bdef\\s+${escapeRegExp(word)}\\s*\\([^\\n]*`),
  );
  if (functionMatch) {
    return functionMatch.trim().replace(/\s*=\s*.*$/, "");
  }

  return undefined;
}

function localHover(source: string, word: string): string | undefined {
  const valueMatch = matchLine(
    source,
    new RegExp(`\\bval\\s+${escapeRegExp(word)}\\s*:\\s*([^=;\\n]+)`),
  );
  if (valueMatch) {
    return valueMatch.trim().replace(/\s*=\s*.*$/, "");
  }

  const inferredValue = matchLine(
    source,
    new RegExp(`\\bval\\s+${escapeRegExp(word)}\\s*=\\s*([0-9]+)`),
  );
  if (inferredValue) {
    return `val ${word}: i32`;
  }

  return undefined;
}

function parameterHover(source: string, word: string): string | undefined {
  const paramPattern = new RegExp(`\\b${escapeRegExp(word)}\\s*:\\s*([^,\\)\\n]+)`);
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

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

import * as cosmo from "../../../packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js";
import * as fs from "fs";
import { resolve } from "path";
import { URI } from "vscode-uri";
import { TextDocuments } from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";

export class CosmoLanguageService {
  readonly releaseDir = resolve("target/cosmo/release");

  readonly system: SystemAdaptor;
  readonly cosmoSystem: any = cosmo.CosmoJsPhysicalSystem();
  readonly cc: Cosmoc;

  constructor(documents: TextDocuments<TextDocument>) {
    this.system = new SystemAdaptor(documents);
    this.cosmoSystem.proxy(freezeThis(this.system, SystemAdaptor));
    this.cc = cosmo.Cosmo(this.cosmoSystem) as any as Cosmoc;

    this.cc.configure(this.releaseDir);

    this.cc.loadPackageByPath(resolve("library/std"));
    this.cc.preloadPackage("std");
  }
}

interface Cosmoc {
  loadPackageByPath(path: string): void;

  service: CosmocService;

  [x: string | number | symbol]: any;
}

interface LspRange {
  start: number;
  end: number;
}

interface HoverResult {
  content: string;
  range: LspRange;
}

interface CosmocService {
  hover(path: string, offset: number): HoverResult;

  [x: string | number | symbol]: any;
}

class SystemAdaptor {
  onReload: (path: string) => void = undefined!;

  constructor(private documents: TextDocuments<TextDocument>) {}

  reload(uri: string): void {
    if (this.onReload) {
      this.onReload(URI.parse(uri).fsPath);
    }
  }

  readFile(path: string): string {
    const uri = URI.file(path).toString();
    const doc = this.documents.get(uri);
    if (doc) {
      console.log("reading file in memory", doc.uri, doc.lineCount);
      return doc.getText();
    }

    return fs.readFileSync(path, "utf-8");
  }

  readDir(path: string): string[] {
    return fs.readdirSync(path); // todo: read directory from memory
  }

  exists(path: string): boolean {
    const uri = URI.file(path).toString();
    const doc = this.documents.get(uri);
    if (doc) {
      console.log("reading exists in memory", doc.uri, doc.lineCount);
      return true;
    }

    return fs.existsSync(path);
  }
}

function freezeThis<T>(ins: T, cls: any): T {
  for (const name of Object.getOwnPropertyNames(Object.getPrototypeOf(ins))) {
    let method = ins[name];
    if (!(!(method instanceof Function) || method === cls)) {
      continue;
    }
    ins[name] = ins[name].bind(ins);
  }

  return ins;
}

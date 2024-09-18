import { type ExtensionContext } from "vscode";
import { activateLsp, deactivateLsp } from "./lsp";

export async function activate(context: ExtensionContext): Promise<void> {
  activateLsp(context);
}

export function deactivate(): void {
  deactivateLsp();
}

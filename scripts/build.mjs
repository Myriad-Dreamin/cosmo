import * as build from "./builders.mjs";

const command = process.argv[2];
const commands = {
  "build:syntax": build.buildSyntax,
  "build:cosmos-host": build.buildCosmosHost,
  "build:vscode": build.buildVscodeExtension,
  "prelaunch:vscode": build.prelaunchVscode,
  "package:vscode": build.packageVscode,
  "install:vscode": build.installVscode,
};

const fn = commands[command];
if (!fn) {
  console.error(`Unknown command: ${command}`);
  process.exit(1);
}

await fn();

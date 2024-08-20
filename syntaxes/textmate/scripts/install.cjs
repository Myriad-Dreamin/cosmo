const fs = require("fs");
const path = require("path");

const filePath = path.join(__dirname, "../cosmo.tmLanguage.json");

const data = fs.readFileSync(filePath, "utf8");

const json = JSON.parse(data);

// todo: make it back when we finished
// delete json.repository.fenced_code_block_typst.patterns;

const outPath = path.join(
  __dirname,
  "../../../editors/vscode/out/cosmo.tmLanguage.json"
);

fs.writeFileSync(outPath, JSON.stringify(json, null, 4), "utf8");
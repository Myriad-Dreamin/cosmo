const fs = require("fs");
const path = require("path");

{
  const filePath = path.join(__dirname, "../cosmo.tmLanguage.json");

  const data = fs.readFileSync(filePath, "utf8");

  const json = JSON.parse(data);

  // todo: make it back when we finished
  // delete json.repository.fenced_code_block_typst.patterns;

  const outPath = path.join(
    __dirname,
    "../../../editors/vscode/out/cosmo.tmLanguage.json"
  );

  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, JSON.stringify(json, null, 4), "utf8");
}
{
  const srcPath = path.join(__dirname, "../cosmo.sublime-syntax");

  let data = fs.readFileSync(srcPath, "utf8");

  data = data.replace(/\?\<\=val\\s\*/g, "?<=val\\s")
    .replace("name: cosmo", "name: cos");

  // (?<=val\s*)

  const outPath = path.join(__dirname, "../../../assets/files/cosmo.sublime-syntax");

  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, data, "utf8");
}
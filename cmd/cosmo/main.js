import * as cosmo from "../../packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js";
import { readFileSync, writeFileSync } from "fs";
import { spawn } from "child_process";
import repl from "repl";

const action = process.argv[2];
const input = process.argv[3];

const compiler = new cosmo.Cosmo();

async function main() {
  if (action === "run") {
    compiler.loadPackageByPath("library/std");
    compiler.preloadPackage("std");
    const executable = compiler.getExecutable(input);

    const asyncSpawn = new Promise((resolve, reject) => {
      if (!executable) return;
      const process = spawn(executable, [], { stdio: "inherit" });
      process.on("close", (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(`Process exited with code ${code}`);
        }
      });
    });

    await asyncSpawn;
  } else if (action === "i") {
    const showError = (result) => {
      if (result.errors) {
        for (const err of result.errors) {
          console.error(err);
        }
      }
    };

    compiler.loadPackageByPath("library/std");
    compiler.preloadPackage("std");

    const env = compiler.createEnv();
    const result = compiler.repl("import _ from std::prelude", env);
    showError(result);
    console.log("REPL started...");

    repl.start({
      prompt: "$ ",
      writer: (t) => t,
      eval(cmd, _context, _filename, callback) {
        if (cmd.trim() === "clear") {
          console.clear();
          callback(null, "");
          return;
        }

        const result = compiler.repl(cmd, env);
        showError(result);
        callback(null, result.result);
      },
    });
  } else if (action === "parse") {
    const inputData = readFileSync(input, "utf8");
    console.log(compiler.parseAsJson(inputData));
  } else {
    const inputData = readFileSync(input, "utf8");
    const output = process.argv[4];
    const outputData = compiler.convert(inputData);

    writeFileSync(output, outputData, "utf8");
  }
}

main().catch(console.error);

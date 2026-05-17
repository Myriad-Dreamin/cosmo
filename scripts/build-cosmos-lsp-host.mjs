import { spawnSync } from "child_process";
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  writeFileSync,
} from "fs";
import { basename, dirname, join, resolve } from "path";

import { ensureNlohmannJsonDependency } from "../cmd/cosmo/main.js";
import * as cosmo from "../packages/cosmo/target/scala-3.3.3/cosmo-opt/main.js";

const repoRoot = resolve(import.meta.dirname, "..");
const packageRoot = join(repoRoot, "packages/cosmos");
const outDir = join(repoRoot, "editors/vscode/out");
const sourcePath = join(outDir, "cosmos-lsp-host.cpp");
const executableName = process.platform === "win32" ? "cosmos-lsp-host.exe" : "cosmos-lsp-host";
const executablePath = join(outDir, executableName);
const logPath = join(outDir, "cosmos-lsp-host.compile.log");

const compiler = new cosmo.Cosmo0();
const compiled = compiler.compilePackageForHost(packageRoot);
if (!compiled.ok) {
  printDiagnostics(compiled.diagnostics);
  throw new Error(`failed to compile packages/cosmos during ${compiled.status}`);
}

mkdirSync(outDir, { recursive: true });
writeFileSync(sourcePath, hostSource(compiled.output), "utf8");

const cxx = findCxxCompiler();
if (!cxx) {
  throw new Error("could not find a C++17 compiler; set CXX or install c++, g++, or clang++");
}

const jsonInclude = resolve(repoRoot, ensureNlohmannJsonDependency());
const supportLibraries = [...(compiled.supportLibraryLinkArguments ?? [])].map((item) => {
  return resolve(repoRoot, item);
});
ensureSupportLibraries(supportLibraries);

const args = [
  "-std=c++17",
  `-I${jsonInclude}`,
  sourcePath,
  ...supportLibraries,
  "-o",
  executablePath,
];
const result = spawnSync(cxx, args, { cwd: repoRoot, encoding: "utf8" });
if (result.status === 0) {
  process.exit(0);
}

const log = [
  `command: ${cxx} ${args.join(" ")}`,
  `status: ${result.status ?? "unknown"}`,
  result.stdout?.trim() ?? "",
  result.stderr?.trim() ?? "",
  result.error?.message ?? "",
]
  .filter(Boolean)
  .join("\n");
writeFileSync(logPath, log, "utf8");
throw new Error(`cosmos LSP host compile failed; log written to ${logPath}`);

function hostSource(generatedSource) {
  return `#define main cosmo0_generated_main
${generatedSource}
#undef main

// VSCode host wrapper for packages/cosmos.
#include <iostream>
#include <limits>

namespace cosmos_vscode_host {
using namespace cosmo0::cosmo_cosmos;

struct OpenDocument {
  std::string uri;
  std::string file_path;
  int32_t version;
  std::string text;
};

inline std::map<std::string, OpenDocument> documents;
inline std::string repo_root;

inline std::string decode_uri_component(const std::string &value) {
  std::string decoded;
  for (std::size_t index = 0; index < value.size(); ++index) {
    if (value[index] != '%' || index + 2 >= value.size()) {
      decoded.push_back(value[index]);
      continue;
    }

    const std::string hex = value.substr(index + 1, 2);
    char *end = nullptr;
    const long parsed = std::strtol(hex.c_str(), &end, 16);
    if (end == nullptr || *end != '\\0') {
      decoded.push_back(value[index]);
      continue;
    }

    decoded.push_back(static_cast<char>(parsed));
    index += 2;
  }
  return decoded;
}

inline std::string file_path_from_uri(const std::string &uri) {
  const std::string prefix = "file://";
  if (uri.rfind(prefix, 0) != 0) {
    return uri;
  }

  std::string path = uri.substr(prefix.size());
  if (path.rfind("/", 0) != 0) {
    path = "/" + path;
  }
  return decode_uri_component(path);
}

inline CosmosTextDocumentSnapshot snapshot(const OpenDocument &document) {
  const std::string marker = "/src/";
  std::string package_root = repo_root + "/packages/cosmos";
  std::string module_path;
  const std::size_t marker_index = document.file_path.rfind(marker);
  if (marker_index != std::string::npos) {
    package_root = document.file_path.substr(0, marker_index);
    module_path = document.file_path.substr(marker_index + marker.size());
    const std::string suffix = ".cos";
    if (module_path.size() >= suffix.size() &&
        module_path.substr(module_path.size() - suffix.size()) == suffix) {
      module_path = module_path.substr(0, module_path.size() - suffix.size());
    }
  }

  return cosmos_text_document_snapshot(
    document.uri,
    document.file_path,
    document.version,
    document.text,
    package_root,
    module_path,
    true
  );
}

inline void write_message(const nlohmann::json &message) {
  const std::string body = message.dump();
  std::cout << "Content-Length: " << body.size() << "\\r\\n\\r\\n" << body;
  std::cout.flush();
}

inline void write_response(const nlohmann::json &id, const nlohmann::json &result) {
  write_message({{"jsonrpc", "2.0"}, {"id", id}, {"result", result}});
}

inline void publish_diagnostics(const OpenDocument &document) {
  const std::string diagnostics_json =
    cosmos_lsp_diagnostics_json(cosmos_analyze_document(snapshot(document)));
  write_message({
    {"jsonrpc", "2.0"},
    {"method", "textDocument/publishDiagnostics"},
    {"params", {
      {"uri", document.uri},
      {"diagnostics", nlohmann::json::parse(diagnostics_json)}
    }}
  });
}

inline bool read_message(std::string &body) {
  std::string line;
  std::size_t content_length = 0;
  while (std::getline(std::cin, line)) {
    if (!line.empty() && line.back() == '\\r') {
      line.pop_back();
    }
    if (line.empty()) {
      break;
    }

    const std::string prefix = "Content-Length:";
    if (line.rfind(prefix, 0) == 0) {
      content_length = static_cast<std::size_t>(std::stoul(line.substr(prefix.size())));
    }
  }

  if (content_length == 0 || !std::cin.good()) {
    return false;
  }

  body.assign(content_length, '\\0');
  std::cin.read(body.data(), static_cast<std::streamsize>(content_length));
  return std::cin.gcount() == static_cast<std::streamsize>(content_length);
}

inline nlohmann::json initialize_result() {
  return {
    {"capabilities", {
      {"textDocumentSync", {{"openClose", true}, {"change", 2}}},
      {"hoverProvider", true},
      {"diagnosticProvider", {{"interFileDependencies", false}, {"workspaceDiagnostics", false}}}
    }},
    {"serverInfo", {{"name", "cosmos"}, {"version", "0.0.0"}}}
  };
}

inline void handle_open(const nlohmann::json &params) {
  const auto &text_document = params.at("textDocument");
  OpenDocument document{
    text_document.at("uri").get<std::string>(),
    file_path_from_uri(text_document.at("uri").get<std::string>()),
    text_document.value("version", 0),
    text_document.value("text", std::string())
  };
  documents[document.uri] = document;
  publish_diagnostics(document);
}

inline void handle_change(const nlohmann::json &params) {
  const auto &text_document = params.at("textDocument");
  const std::string uri = text_document.at("uri").get<std::string>();
  auto found = documents.find(uri);
  if (found == documents.end()) {
    return;
  }

  found->second.version = text_document.value("version", found->second.version);
  const auto &changes = params.at("contentChanges");
  if (!changes.empty()) {
    found->second.text = changes.back().value("text", found->second.text);
  }
  publish_diagnostics(found->second);
}

inline void handle_close(const nlohmann::json &params) {
  const std::string uri = params.at("textDocument").at("uri").get<std::string>();
  documents.erase(uri);
  write_message({
    {"jsonrpc", "2.0"},
    {"method", "textDocument/publishDiagnostics"},
    {"params", {{"uri", uri}, {"diagnostics", nlohmann::json::array()}}}
  });
}

inline void handle_hover(const nlohmann::json &id, const nlohmann::json &params) {
  const std::string uri = params.at("textDocument").at("uri").get<std::string>();
  const auto found = documents.find(uri);
  if (found == documents.end()) {
    write_response(id, nullptr);
    return;
  }

  const auto &position = params.at("position");
  const auto hover = cosmos_hover(
    snapshot(found->second),
    position.value("line", 0),
    position.value("character", 0)
  );
  write_response(id, nlohmann::json::parse(cosmos_hover_result_json(hover)));
}

inline void handle_document_diagnostic(const nlohmann::json &id, const nlohmann::json &params) {
  const std::string uri = params.at("textDocument").at("uri").get<std::string>();
  const auto found = documents.find(uri);
  if (found == documents.end()) {
    write_response(id, {{"kind", "full"}, {"items", nlohmann::json::array()}});
    return;
  }

  const std::string diagnostics_json =
    cosmos_lsp_diagnostics_json(cosmos_analyze_document(snapshot(found->second)));
  write_response(id, {{"kind", "full"}, {"items", nlohmann::json::parse(diagnostics_json)}});
}

inline bool dispatch(const nlohmann::json &message) {
  const std::string method = message.value("method", std::string());
  const nlohmann::json params = message.value("params", nlohmann::json::object());

  if (method == "initialize") {
    write_response(message.at("id"), initialize_result());
    return true;
  }
  if (method == "shutdown") {
    write_response(message.at("id"), nullptr);
    return true;
  }
  if (method == "exit") {
    return false;
  }
  if (method == "textDocument/didOpen") {
    handle_open(params);
    return true;
  }
  if (method == "textDocument/didChange") {
    handle_change(params);
    return true;
  }
  if (method == "textDocument/didClose") {
    handle_close(params);
    return true;
  }
  if (method == "textDocument/hover") {
    handle_hover(message.at("id"), params);
    return true;
  }
  if (method == "textDocument/diagnostic") {
    handle_document_diagnostic(message.at("id"), params);
    return true;
  }

  if (message.contains("id")) {
    write_message({
      {"jsonrpc", "2.0"},
      {"id", message.at("id")},
      {"error", {{"code", -32601}, {"message", "method not found"}}}
    });
  }
  return true;
}

inline int run() {
  const char *root = std::getenv("COSMO_REPO_ROOT");
  repo_root = root == nullptr ? std::string() : std::string(root);

  std::string body;
  while (read_message(body)) {
    if (!dispatch(nlohmann::json::parse(body))) {
      return 0;
    }
  }
  return 0;
}

} // namespace cosmos_vscode_host

int main(int argc, char **argv) {
  if (argc > 1 && std::string(argv[1]) == "probe") {
    return 0;
  }
  return cosmos_vscode_host::run();
}
`;
}

function findCxxCompiler() {
  const candidates = [process.env.CXX, "c++", "g++", "clang++"].filter(Boolean);
  for (const candidate of candidates) {
    const result = spawnSync(candidate, ["--version"], { encoding: "utf8" });
    if (result.status === 0) {
      return candidate;
    }
  }
  return undefined;
}

function ensureSupportLibraries(linkArguments) {
  for (const argument of linkArguments) {
    if (existsSync(argument)) {
      continue;
    }

    const id = supportLibraryIdFromPath(argument);
    const result = spawnSync("cargo", ["build", "--release", "-p", id], {
      cwd: repoRoot,
      encoding: "utf8",
    });
    if (result.status !== 0) {
      throw new Error(
        [`cargo build --release -p ${id} failed`, result.stdout, result.stderr]
          .filter(Boolean)
          .join("\n"),
      );
    }

    const source = join(repoRoot, "target", "release", `libcosmo_${id.replaceAll("-", "_")}.a`);
    if (!existsSync(source)) {
      throw new Error(`support library build did not produce ${source}`);
    }

    mkdirSync(dirname(argument), { recursive: true });
    copyFileSync(source, argument);
  }
}

function supportLibraryIdFromPath(value) {
  const parent = basename(dirname(value));
  if (!/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/.test(parent)) {
    throw new Error(`cannot infer support-library id from ${value}`);
  }
  return parent;
}

function printDiagnostics(diagnostics) {
  for (const diagnostic of diagnostics) {
    console.error(`${diagnostic.phase} ${diagnostic.severity} ${diagnostic.code}: ${diagnostic.message}`);
  }
}

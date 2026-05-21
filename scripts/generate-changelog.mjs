#!/usr/bin/env node

import { execFileSync } from "child_process";
import fs from "fs";

const args = process.argv.slice(2);
let targetArg = "";
let outputJson = false;
let write = false;
let previousTag = "";

for (let index = 0; index < args.length; index += 1) {
  const arg = args[index];
  if (arg === "--json") {
    outputJson = true;
    continue;
  }

  if (arg === "--write") {
    write = true;
    continue;
  }

  if (arg === "--previous-tag") {
    previousTag = args[index + 1] ?? "";
    if (!previousTag) {
      usage("--previous-tag requires a tag");
    }
    index += 1;
    continue;
  }

  if (!targetArg) {
    targetArg = arg;
    continue;
  }

  usage(`Unexpected argument: ${arg}`);
}

if (!targetArg) {
  usage("Missing target version");
}

const targetVersion = normalizeVersion(targetArg);
const targetTag = `v${targetVersion}`;
const notesTag = `v${stripReleaseCandidate(targetVersion)}`;
const repository = githubRepository();
const generatedNotes = fetchGeneratedNotes(repository, notesTag, previousTag);
const entry = renderEntry(targetVersion, generatedNotes);
const changelog = upsertChangelogEntry("CHANGELOG.md", targetVersion, entry);

const result = {
  targetVersion,
  targetTag,
  notesTag,
  repository,
  previousTag: previousTag || null,
  itemCount: parseGeneratedItems(generatedNotes).length,
  changelog,
};

if (write) {
  fs.writeFileSync("CHANGELOG.md", changelog.content, "utf8");
}

if (outputJson) {
  process.stdout.write(`${JSON.stringify(result, null, 2)}\n`);
} else if (write) {
  console.log(`Updated CHANGELOG.md for ${targetTag}`);
} else {
  process.stdout.write(changelog.content);
}

function usage(message) {
  if (message) {
    console.error(message);
  }
  console.error(
    "Usage: node scripts/generate-changelog.mjs <version> [--previous-tag <tag>] [--write] [--json]",
  );
  process.exit(1);
}

function normalizeVersion(version) {
  return version.startsWith("v") ? version.slice(1) : version;
}

function stripReleaseCandidate(version) {
  return version.replace(/-rc[1-9]\d*$/, "");
}

function githubRepository() {
  if (process.env.GITHUB_REPOSITORY) {
    return process.env.GITHUB_REPOSITORY;
  }

  const remote = readGitRemote();
  const match = remote.match(/github\.com[:/]([^/]+\/[^/.]+)(?:\.git)?$/);
  if (!match) {
    throw new Error(`Could not infer GitHub repository from origin: ${remote}`);
  }
  return match[1];
}

function readGitRemote() {
  try {
    return execFileSync("git", ["remote", "get-url", "origin"], {
      encoding: "utf8",
    }).trim();
  } catch (error) {
    const stdout = error.stdout?.toString().trim();
    if (stdout) {
      return stdout;
    }
    throw error;
  }
}

function fetchGeneratedNotes(repository, tagName, previousTagName) {
  const args = [
    "api",
    `repos/${repository}/releases/generate-notes`,
    "-f",
    `tag_name=${tagName}`,
  ];
  if (previousTagName) {
    args.push("-f", `previous_tag_name=${previousTagName}`);
  }
  args.push("--jq", ".body");

  return execFileSync("gh", args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });
}

function renderEntry(version, notes) {
  const items = parseGeneratedItems(notes);
  const grouped = groupItems(items);
  const lines = [`## v${version} - [${today()}]`, ""];

  for (const [section, sectionItems] of grouped) {
    if (sectionItems.length === 0) {
      continue;
    }

    lines.push(`### ${section}`, "");
    for (const item of sectionItems) {
      lines.push(`* ${formatItem(item)}`);
    }
    lines.push("");
  }

  const fullChangelog = fullChangelogLine(notes, version);
  if (fullChangelog) {
    lines.push(fullChangelog);
    lines.push("");
  }

  return lines.join("\n");
}

function parseGeneratedItems(notes) {
  return notes
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line.startsWith("* "))
    .filter((line) => !line.includes(" made their first contribution in "))
    .map((line) => parseGeneratedItem(line.slice(2)));
}

function parseGeneratedItem(line) {
  const match = line.match(/^(.*?) by (@[^\s]+) in (https:\/\/github\.com\/[^\s]+)$/);
  if (!match) {
    return {
      raw: line,
      title: line,
      author: "",
      url: "",
      type: "",
      scope: "",
      subject: line,
    };
  }

  const parsed = parseConventionalTitle(match[1]);
  return {
    raw: line,
    title: match[1],
    author: match[2],
    url: match[3],
    ...parsed,
  };
}

function parseConventionalTitle(title) {
  const match = title.match(/^([a-z]+)(?:\(([^)]+)\))?!?:\s*(.+)$/i);
  if (!match) {
    return {
      type: "",
      scope: "",
      subject: title,
    };
  }

  const nested = match[3].match(/^([a-z]+)(?:\(([^)]+)\))?!?:\s*(.+)$/i);
  if (nested) {
    return {
      type: nested[1].toLowerCase(),
      scope: nested[2] ?? match[2] ?? "",
      subject: nested[3],
    };
  }

  return {
    type: match[1].toLowerCase(),
    scope: match[2] ?? "",
    subject: match[3],
  };
}

function groupItems(items) {
  const sections = new Map([
    ["Compiler", []],
    ["Bootstrap Runtime", []],
    ["Language Server and Editor", []],
    ["CLI and Tooling", []],
    ["Testing and Documentation", []],
    ["Misc", []],
  ]);

  for (const item of items) {
    sections.get(sectionFor(item)).push(item);
  }

  return [...sections.entries()].filter(([, sectionItems]) => sectionItems.length > 0);
}

function sectionFor(item) {
  const text = `${item.scope} ${item.subject}`.toLowerCase();
  if (/\b(github pages|gh pages|pnpm|ci|build)\b/.test(text)) {
    return "CLI and Tooling";
  }

  if (/\b(vscode|language server|lsp|hover|diagnostic|diagnostics|definition|reference|workspace|jsonrpc|cosmos)\b/.test(text)) {
    return "Language Server and Editor";
  }

  if (/\b(cosmoc|cosmo1|parser|syntax|typecheck|type-check|type model|ir|name resolution|module graph|type inference|dispatching|pattern match|compile-time|mutable refs)\b/.test(text)) {
    return "Compiler";
  }

  if (/\b(cosmo0|core0|lir|cpp backend|extern abi|runtime|package pipeline|capability|support library)\b/.test(text)) {
    return "Bootstrap Runtime";
  }

  if (/\b(test|fixture|corpus|sample|docs|documentation|spec)\b/.test(text)) {
    return "Testing and Documentation";
  }

  if (/\b(cli|cmake|linker|bootstrap|github pages|gh pages|ci|build|pnpm)\b/.test(text)) {
    return "CLI and Tooling";
  }

  return "Misc";
}

function formatItem(item) {
  const prefix = itemPrefix(item.type);
  const subject = sentenceCase(cleanSubject(item.subject));
  const author = item.author && item.author !== "@Myriad-Dreamin" ? ` by ${item.author}` : "";
  const location = item.url ? ` in ${item.url}` : "";
  return `${prefix}${subject}${author}${location}`;
}

function itemPrefix(type) {
  if (type === "fix") {
    return "(Fix) ";
  }
  if (type === "perf") {
    return "(Perf) ";
  }
  if (type === "test") {
    return "(Test) ";
  }
  if (type === "docs") {
    return "(Docs) ";
  }
  return "";
}

function cleanSubject(subject) {
  return subject
    .replace(/^add\b/i, "Added")
    .replace(/^start\b/i, "Started")
    .replace(/^split\b/i, "Split")
    .replace(/^lower\b/i, "Lowered")
    .replace(/^typecheck\b/i, "Typechecked")
    .replace(/^type-check\b/i, "Type-checked")
    .replace(/^resolve\b/i, "Resolved")
    .replace(/^merge\b/i, "Merged")
    .replace(/^complete\b/i, "Completed")
    .replace(/^accept\b/i, "Accepted")
    .replace(/^install\b/i, "Installed")
    .replace(/^update\b/i, "Updated")
    .replace(/^refresh\b/i, "Refreshed")
    .replace(/^correct\b/i, "Corrected")
    .replace(/^fix\b/i, "Fixed");
}

function sentenceCase(text) {
  if (!text) {
    return text;
  }
  return text[0].toUpperCase() + text.slice(1);
}

function fullChangelogLine(notes, version) {
  const match = notes.match(/\*\*Full Changelog\*\*: (https:\/\/github\.com\/[^\s]+)/);
  if (!match) {
    return `**Full Changelog**: https://github.com/${githubRepository()}/commits/v${version}`;
  }

  return match[0].replace(/v\d+\.\d+\.\d+(?:-rc[1-9]\d*)?$/, `v${version}`);
}

function upsertChangelogEntry(filePath, version, entry) {
  const intro = changelogIntro();
  if (!fs.existsSync(filePath)) {
    return {
      status: "created",
      content: `${intro}\n${entry}`,
    };
  }

  const content = fs.readFileSync(filePath, "utf8");
  const headingPattern = new RegExp(`^## v${escapeRegExp(version)}\\b.*$`, "m");
  const heading = content.match(headingPattern);
  if (!heading) {
    const insertion = content.match(/^## v/m);
    if (!insertion) {
      return {
        status: "appended",
        content: `${content.trimEnd()}\n\n${entry}`,
      };
    }

    return {
      status: "inserted",
      content: `${content.slice(0, insertion.index)}${entry}\n${content.slice(insertion.index)}`,
    };
  }

  const start = heading.index;
  const nextHeading = content.slice(start + heading[0].length).match(/\n## v/);
  const end = nextHeading ? start + heading[0].length + nextHeading.index + 1 : content.length;
  return {
    status: "replaced",
    content: `${content.slice(0, start)}${entry}${content.slice(end)}`,
  };
}

function changelogIntro() {
  return [
    "# Change Log",
    "",
    'All notable changes to "cosmo" will be documented in this file.',
    "",
    "The changelog lines unspecified with authors are all written by the @Myriad-Dreamin.",
    "",
  ].join("\n");
}

function today() {
  return new Date().toISOString().slice(0, 10);
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

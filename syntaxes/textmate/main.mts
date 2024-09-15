import * as textmate from "./textmate.mjs";

import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "node:url";

const blockComment: textmate.Pattern = {
  name: "comment.block.cosmo",
  begin: /\/\*/,
  end: /\*\//,
  beginCaptures: {
    "0": {
      name: "punctuation.definition.comment.cosmo",
    },
  },
  patterns: [
    {
      include: "#blockComment",
    },
  ],
};

const lineComment: textmate.Pattern = {
  name: "comment.line.double-slash.cosmo",
  begin: /\/\//,
  end: /(?=$|\n)/,
  beginCaptures: {
    "0": {
      name: "punctuation.definition.comment.cosmo",
    },
  },
};

const IDENTIFIER = /\b[\p{XID_Start}_][\p{XID_Continue}_]*/u;

const identifier: textmate.PatternMatch = {
  match: IDENTIFIER,
  name: "variable.other.readwrite.cosmo",
};

const escape: textmate.Pattern = {
  name: "constant.character.escape.cosmo",
  match: /\\./,
};

const stringPattern = (p: RegExp, escape = false): textmate.Pattern => ({
  name: "string.quoted.double.cosmo",
  begin: p.source + `(?=$|[^"]|${p.source})`,
  end: p,
  beginCaptures: {
    "0": {
      name: "punctuation.definition.string.begin.cosmo",
    },
  },
  endCaptures: {
    "0": {
      name: "punctuation.definition.string.end.cosmo",
    },
  },
  patterns: [...(escape ? [{ include: "#escape" }] : [])],
});

const codePattern = (p: RegExp): textmate.Pattern => ({
  begin: `\\b(code)\\((${p.source}(?=$|[^"]|${p.source}))`,
  end: `(${p.source})\\)`,
  beginCaptures: {
    "1": {
      name: "entity.name.function.cosmo",
    },
    "2": {
      name: "punctuation.definition.string.begin.cosmo",
    },
  },
  endCaptures: {
    "1": {
      name: "punctuation.definition.string.end.cosmo",
    },
  },
  contentName: "meta.embedded.cpp",
  patterns: [{ include: "source.cpp" }],
});

const constPre = /(?<=val\s*|\:\:)/.source + IDENTIFIER.source;
const constPost = IDENTIFIER.source + /(?=\:\:)/.source;
const constIdentifier: textmate.PatternMatch = {
  match: new RegExp(`(?:\\b_\\b)|(?:${constPre})|(?:${constPost})`),
  name: "entity.name.type.cosmo",
};

const funcPre = /(?<=def\s*)/.source + IDENTIFIER.source;
const functionPreIdentifier: textmate.PatternMatch = {
  match: funcPre,
  name: "entity.name.function.cosmo",
};

const funcPre2 = /(?<=\@)/.source + IDENTIFIER.source;
const funcPost = IDENTIFIER.source + /(?=[\(\[]|\s*\{)/.source;
const functionPostIdentifier: textmate.PatternMatch = {
  match: new RegExp(`(?:${funcPre2})|(?:${funcPost})`),
  name: "entity.name.function.cosmo",
};

const typeConvention = /(?<!\.)(?=[A-Z])/.source + IDENTIFIER.source;

const typeIdentifier: textmate.PatternMatch = {
  match:
    `(?:${typeConvention})|` +
    /(?<!\.)\b(?:any|never|bool|str|f32|f64|f128|i8|i16|i32|i64|i128|u8|u16|u32|u64|u128|isize|usize)\b/
      .source,
  name: "entity.name.type.cosmo",
};

const comments: textmate.Pattern = {
  patterns: [{ include: "#blockComment" }, { include: "#lineComment" }],
};

const contextualKeywords: textmate.Pattern = {
  patterns: [
    {
      name: "keyword.control.cosmo",
      match: /\b(?:from)(?=\s*["\p{XID_Start}_])\b/u,
    },
  ],
};

const keywords: textmate.Pattern = {
  patterns: [
    {
      name: "keyword.control.cosmo",
      match:
        /\b(?:extern|pub|mut|private|impl|yield|lazy|as|import|module|unsafe|match|implicit|break|continue|using|throw|return|case|def|Self|self|super|class|trait|type|if|else|for|while|loop|val|var|and|or|in|not)\b/,
    },
  ],
};

const hexNumeric: textmate.Pattern = {
  name: "constant.numeric.cosmo",
  match: /\b0x[\da-fA-F]+\b/,
};

const octalNumeric: textmate.Pattern = {
  name: "constant.numeric.cosmo",
  match: /\b0o[0-7]+\b/,
};

const numeric: textmate.Pattern = {
  name: "constant.numeric.cosmo",
  match: /\b(?:0x[\da-fA-F]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)\b/,
};

const markers: textmate.Pattern = {
  name: "keyword.control.cosmo",
  match: /=>|\?\?\?/,
};

const literal: textmate.Pattern = {
  name: "constant.language.cosmo",
  match: /\b(?:true|false|none)\b/,
};

export const cosmo: textmate.Grammar = {
  repository: {
    comments,
    blockComment,
    lineComment,
    stringPattern: stringPattern(/"/, true),
    stringPattern3: stringPattern(/"""/),
    stringPattern5: stringPattern(/"""""/),
    stringPattern7: stringPattern(/"""""""/),
    stringPattern9: stringPattern(/"""""""""/),
    stringPattern11: stringPattern(/"""""""""""/),
    codePattern3: codePattern(/"""/),
    codePattern5: codePattern(/"""""/),
    codePattern7: codePattern(/"""""""/),
    codePattern9: codePattern(/"""""""""/),
    codePattern11: codePattern(/"""""""""""/),
    markers,
    literal,
    contextualKeywords,
    keywords,
    typeIdentifier,
    constIdentifier,
    functionPreIdentifier,
    functionPostIdentifier,
    identifier,
    escape,
    hexNumeric,
    octalNumeric,
    numeric,
  },
};

function generate() {
  const dirname = fileURLToPath(new URL(".", import.meta.url));

  const compiled = textmate.compile(cosmo);
  const repository = JSON.parse(compiled).repository;

  // dump to file
  fs.writeFileSync(
    path.join(dirname, "../cosmo.tmLanguage.json"),
    JSON.stringify({
      $schema:
        "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
      scopeName: "source.cosmo",
      name: "cosmo",
      patterns: [
        {
          include: "#comments",
        },
        {
          include: "#codePattern11",
        },
        {
          include: "#codePattern9",
        },
        {
          include: "#codePattern7",
        },
        {
          include: "#codePattern5",
        },
        {
          include: "#codePattern3",
        },
        {
          include: "#stringPattern11",
        },
        {
          include: "#stringPattern9",
        },
        {
          include: "#stringPattern7",
        },
        {
          include: "#stringPattern5",
        },
        {
          include: "#stringPattern3",
        },
        {
          include: "#stringPattern",
        },
        {
          include: "#literal",
        },
        {
          include: "#markers",
        },
        {
          include: "#contextualKeywords",
        },
        {
          include: "#keywords",
        },
        {
          include: "#typeIdentifier",
        },
        {
          include: "#functionPostIdentifier",
        },
        {
          include: "#constIdentifier",
        },
        {
          include: "#functionPreIdentifier",
        },
        {
          include: "#identifier",
        },
        {
          include: "#hexNumeric",
        },
        {
          include: "#octalNumeric",
        },
        {
          include: "#numeric",
        },
      ],
      repository,
    })
  );
}

generate();

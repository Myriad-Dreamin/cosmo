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

const stringPattern: textmate.Pattern = {
  name: "string.quoted.double.cosmo",
  begin: /"/,
  end: /"/,
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
  patterns: [
    {
      include: "#escape",
    },
  ],
};

const constIdentifier: textmate.PatternMatch = {
  match: /(?<=val\s*)/.source + IDENTIFIER.source,
  name: "entity.name.type.cosmo",
};

const functionIdentifier: textmate.PatternMatch = {
  match: IDENTIFIER.source + /(?=\(|\s*\{)/.source,
  name: "entity.name.function.cosmo",
};

const typeConvention = /(?=[A-Z])/.source + IDENTIFIER.source;

const typeIdentifier: textmate.PatternMatch = {
  match:
    `(?:${typeConvention})|` +
    /\b(?:any|none|bool|f32|f64|f128|i8|i16|i32|i64|i128|u8|u16|u32|u64|u128|isize|usize)\b/
      .source,
  name: "entity.name.type.cosmo",
};

const comments: textmate.Pattern = {
  patterns: [{ include: "#blockComment" }, { include: "#lineComment" }],
};

const keywords: textmate.Pattern = {
  patterns: [
    {
      name: "keyword.control.cosmo",
      match:
        /\b(?:pub|private|lazy|as|import|module|unsafe|match|implicit|break|continue|using|throw|return|case|def|self|class|trait|type|if|else|for|loop|val|var|and|or|in|not)\b/,
    },
  ],
};

const numeric: textmate.Pattern = {
  name: "constant.numeric.cosmo",
  match: /\b(?:0x[\da-fA-F]+|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)\b/,
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
    stringPattern,
    literal,
    keywords,
    typeIdentifier,
    functionIdentifier,
    constIdentifier,
    identifier,
    escape,
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
          include: "#stringPattern",
        },
        {
          include: "#literal",
        },
        {
          include: "#keywords",
        },
        {
          include: "#typeIdentifier",
        },
        {
          include: "#constIdentifier",
        },
        {
          include: "#functionIdentifier",
        },
        {
          include: "#identifier",
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

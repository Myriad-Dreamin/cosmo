#!/usr/bin/env python3
# https://github.com/twolfson/sublime-files/blob/9b4b85a94820e6af60d3cc635d479408c56e49fc/Packages/Default/convert_syntax.py

import os
import json
import sys

try:
    import yaml
except ImportError:
    pass


def needs_yaml_quoting(s):
    return (
        s == "" or
        s[0] in "\"'%-:?@`&*!,#|>0123456789=" or
        s.startswith("<<") or
        s in ["true", "false", "null"] or
        "# " in s or
        ": " in s or
        "[" in s or
        "]" in s or
        "{" in s or
        "}" in s or
        "\n" in s or
        s[-1] in ":#" or
        s.strip() != s)


def quote(s):
    if s.count("'") <= len(s) / 2:
        return "'" + s.replace("'", "''") + "'"
    else:
        return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def order_keys(l):
    key_order = reversed([
        "name", "main", "match", "comment", "file_extensions",
        "first_line_match", "hidden", "match", "scope", "main"])
    l = sorted(l)
    for key in key_order:
        if key in l:
            del l[l.index(key)]
            l.insert(0, key)
    return l


def to_yaml(val, start_block_on_newline=False, indent=0):
    tab_size = 2
    out = ""

    if indent == 0:
        out += "%YAML 1.2\n---\n"
        out += "# http://www.sublimetext.com/docs/syntax.html\n"

    if isinstance(val, list):
        if len(val) == 0:
            out += "[]\n"
        else:
            if start_block_on_newline:
                out += '\n'

            for x in val:
                out += " " * indent
                out += "- "
                out += to_yaml(x, False, indent + 2)
    elif isinstance(val, dict):
        if start_block_on_newline:
            out += '\n'

        first = True
        for k in order_keys(val.keys()):
            v = val[k]

            if not first or start_block_on_newline:
                out += " " * indent
            else:
                first = False

            if isinstance(k, int):
                out += str(k)
            elif needs_yaml_quoting(k):
                out += quote(k)
            else:
                out += k

            out += ": "
            out += to_yaml(v, True, indent + tab_size)
    elif isinstance(val, str):
        if needs_yaml_quoting(val):
            if "\n" in val:
                assert(start_block_on_newline)
                if start_block_on_newline:
                    if val[-1] == "\n":
                        out += '|\n'
                    else:
                        out += "|-\n"
                for l in val.splitlines():
                    out += " " * indent
                    out += l
                    out += "\n"
            else:
                out += quote(val)
                out += "\n"
        else:
            out += val
            out += "\n"
    elif isinstance(val, bool):
        if val:
            out += "true\n"
        else:
            out += "false\n"
    else:
        out += str(val) + "\n"
    return out


def build_scope_map():
    syntax_by_scope = {}
    for f in sublime.find_resources("*.tmLanguage.json"):
        try:
            l = sublime.load_resource(f)
            if "scopeName" in l:
                fname = os.path.splitext(f)[0] + ".sublime-syntax"
                syntax_by_scope[l["scopeName"]] = os.path.basename(fname)
        except:
            pass

    return syntax_by_scope


scope_map = {}


def syntax_for_scope(key):
    use_scope_refs = True
    if use_scope_refs:
        return "scope:" + key
    else:
        global scope_map
        if len(scope_map) == 0:
            scope_map = build_scope_map()

        return scope_map[key]


def is_external_syntax(key):
    return key[0] not in "#$"


def format_external_syntax(key):
    assert(is_external_syntax(key))

    if '#' in key:
        syntax, rule = key.split('#')
        return syntax_for_scope(syntax) + '#' + rule
    else:
        return syntax_for_scope(key)


def leading_whitespace(s):
    return s[0:len(s) - len(s.lstrip())]


def format_regex(s):
    if "\n" in s:
        lines = s.splitlines()

        # trim common indentation off of each line
        if len(lines) > 1:
            common_indent = leading_whitespace(lines[1])
            for l in lines[2:]:
                cur_indent = leading_whitespace(l)
                if cur_indent.startswith(common_indent):
                    pass
                elif common_indent.startswith(cur_indent):
                    common_indent = cur_indent
                else:
                    common_indent = ""

            # Generally the first line doesn't have any indentation, add some
            if not lines[0].startswith(common_indent):
                lines[0] = common_indent + lines[0].lstrip()
        else:
            common_indent = leading_whitespace(lines[0])

        s = "\n".join([l[len(common_indent):] for l in lines]).rstrip("\n")

    return s


def format_comment(s):
    s = s.strip().replace("\t", "    ")

    if "\n" in s:
        s = s.rstrip("\n") + "\n"

    return s


def format_captures(c):
    ret = {}
    for k, v in c.items():
        if "name" not in v:
            warn("patterns and includes are not supported within captures: " + str(c))
            continue

        try:
            ret[int(k)] = v["name"]
        except ValueError:
            warn("named capture used, this is unsupported")
            ret[k] = v["name"]
    return ret


def warn(msg):
    print(msg, file=sys.stderr)


def make_context(patterns, repository):
    ctx = []
    for p in patterns:
        if "begin" in p:
            entry = {}
            entry["match"] = format_regex(p["begin"])
            if "beginCaptures" in p or "captures" in p:
                if "beginCaptures" in p:
                    captures = format_captures(p["beginCaptures"])
                else:
                    captures = format_captures(p["captures"])

                if "0" in captures:
                    entry["scope"] = captures["0"]
                    del captures["0"]

                if len(captures) > 0:
                    entry["captures"] = captures

            end_entry = {}
            end_entry["match"] = format_regex(p["end"])
            end_entry["pop"] = True
            if "endCaptures" in p or "captures" in p:
                captures = format_captures(
                    p.get("endCaptures", p.get("captures")))
                if "0" in captures:
                    end_entry["scope"] = captures["0"]
                    del captures["0"]

                if len(captures) > 0:
                    end_entry["captures"] = captures

            if "\\G" in end_entry["match"]:
                warn(
                    "pop pattern contains \\G, this will not work as expected"
                    " if it's intended to refer to the begin regex: " +
                    end_entry["match"])

            apply_last = False
            if "applyEndPatternLast" in p and p["applyEndPatternLast"] == 1:
                apply_last = True

            if "patterns" in p:
                child_patterns = p["patterns"]
            else:
                child_patterns = []

            child = make_context(child_patterns, repository)
            if apply_last:
                child.append(end_entry)
            else:
                child.insert(0, end_entry)

            if "contentName" in p:
                child.insert(0, {"meta_content_scope": p["contentName"]})
            if "name" in p:
                child.insert(0, {"meta_scope": p["name"]})

            if "comment" in p:
                comment = format_comment(p["comment"])
                if len(comment) > 0:
                    entry["comment"] = comment

            entry["push"] = child

            ctx.append(entry)

        elif "match" in p:
            entry = {}
            entry["match"] = format_regex(p["match"])

            if "name" in p:
                entry["scope"] = p["name"]

            if "captures" in p:
                entry["captures"] = format_captures(p["captures"])

            if "comment" in p:
                comment = format_comment(p["comment"])
                if len(comment) > 0:
                    entry["comment"] = comment

            ctx.append(entry)

        elif "include" in p:
            key = p["include"]

            if key[0] == '#':
                key = key[1:]
                if key not in repository:
                    raise Exception("no entry in repository for " + key)

                ctx.append({"include": key})
            elif key == "$self":
                ctx.append({"include": "main"})
            elif key == "$base":
                ctx.append({"include": "$top_level_main"})
            elif key[0] == '$':
                raise Exception("unknown include: " + key)
            else:
                # looks like an external include
                # assert(is_external_syntax(key))
                # ctx.append({"include-syntax": key})
                ctx.append({"include": format_external_syntax(key)})

        else:
            raise Exception("unknown pattern type: %s" % p.keys())

    return ctx


def convert(fname):
    with open(fname, 'r', encoding='utf-8') as f:
        l = json.load(f)

    if "repository" in l:
        repository = l["repository"]
    else:
        repository = {}

    # normalize the repository values into being a list of patterns
    for key, value in repository.items():
        if "begin" in value or "match" in value:
            repository[key] = [value]
        else:
            repository[key] = value["patterns"]

    contexts = {"main": make_context(l["patterns"], repository)}

    for key, value in repository.items():
        assert(key != "main")

        contexts[key] = make_context(value, repository)

    syn = {}

    if "comment" in l:
        comment = format_comment(l["comment"])
        if len(comment) > 0:
            syn["comment"] = comment

    if "name" in l:
        syn["name"] = l["name"]

    if "scopeName" in l:
        syn["scope"] = l["scopeName"]

    if "fileTypes" in l:
        syn["file_extensions"] = l["fileTypes"]

    if "firstLineMatch" in l:
        syn["first_line_match"] = l["firstLineMatch"]

    if "hideFromUser" in l:
        syn["hidden"] = l["hideFromUser"]

    if "hidden" in l:
        syn["hidden"] = l["hidden"]
    elif fname.endswith('.hidden-tmLanguage'):
        syn["hidden"] = True

    syn["contexts"] = contexts

    return syn


def extract_by_key(key, v):
    if isinstance(v, list):
        ret = []
        for x in v:
            ret.extend(extract_by_key(key, x))
        return ret
    elif isinstance(v, dict):
        ret = []
        for k, x in v.items():
            if k == key:
                ret.append(x)
            else:
                ret.extend(extract_by_key(key, x))
        return ret
    else:
        return []


def find_external_refs():
    refmap = {}
    for f in sublime.find_resources("*.tmLanguage.json"):
        l = convert(f)
        refmap[f] = extract_by_key("syntax", l)
    print(to_yaml(refmap, False, 0))


import fnmatch

class sublime:
    base_path = "."

    @classmethod
    def find_resources(cls, pattern):
        paths = []
        for root, dirs, files in os.walk(cls.base_path):
            for fname in files:
                if fnmatch.fnmatch(fname, pattern):
                    path = os.path.join(root, fname)

                    if path[0:2] == "./" or path[0:2] == ".\\":
                        path = path[2:]

                    paths.append(path)
        return paths

    @staticmethod
    def load_resource(fname):
        with open(fname, 'r', encoding='utf-8') as f:
            l = json.load(f)

if __name__ == "__main__":
    args = sys.argv[1:]

    if not args:
        print("usage: convert_syntax.py files")
        print("       convert_syntax.py folder")
    else:
        filenames = []
        for path in args:
            if os.path.isdir(path):
                sublime.base_path = path
                filenames.extend(sublime.find_resources("*.tmLanguage.json"))
            else:
                filenames.append(path)

        for fname in filenames:
            fv = os.path.splitext(fname)[0]
            fv = os.path.splitext(fv)[0]
            outfile = fv + ".sublime-syntax"
            # if os.path.exists(outfile):
            #     print("file already exists: " + outfile)
            #     continue

            data = convert(fname)
            text = to_yaml(data)
            # verify that to_yaml produces valid yaml for this object
            if 'yaml' in sys.modules:
                assert(data == yaml.safe_load(text))

            with open(outfile, "w", encoding="utf-8") as f:
                # to_yaml will leave some trailing whitespace, remove it
                text = "\n".join(l.rstrip() for l in text.splitlines()) + "\n"
                f.write(text)

            print("converted " + outfile)
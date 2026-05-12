= cosmo0 Control Flow

== Status

This file owns branch, loop, and return forms for cosmo0. The section headings are initial ownership markers.

== Blocks and Sequencing

Placeholder for ordered execution, scope creation, expression statements, and statement/result boundaries.

== Examples

Accepted control-flow shape:

```cos
def find_eof(tokens: Vec<Token>): usize = {
  var i: usize = 0
  while i < tokens.len() {
    val token = tokens.get(i)
    if token.kind == TokenKind.Eof {
      return i
    }
    i = i + 1
  }
  return tokens.len()
}
```

Candidate `for` shape once the owning standard API explicitly admits `Vec<T>` iteration:

```cos
for token in tokens {
  if token.kind == TokenKind.Eof {
    return token.span.start
  }
}
```

Rejected control-flow shape until later specs admit it:

```cos
async def read_tokens(path: Path): Vec<Token> = {
  await Fs.read_to_string(path)
}
```

The accepted example uses local mutation, `while`, `if`, and `return`. The `for` example is intentionally labeled as capability-dependent because iteration is accepted only for documented standard iterable APIs.

== If and Else

Placeholder for condition typing, branch typing, required else behavior, and diagnostics for unsupported conditional forms.

== Loops

Placeholder for accepted `loop` and `while` forms, loop body typing, and loop-result restrictions.

== For Iteration

Placeholder for `for` iteration over explicitly supported standard iterable APIs. Iteration over undocumented types is rejected even if an implementation could lower it.

== Break, Continue, and Return

Placeholder for control-transfer validity, target resolution, return typing, and unreachable-code policy.

== Rejected Control Flow

Placeholder for generators, coroutines, async control flow, non-local exits, and other full-language forms outside cosmo0.

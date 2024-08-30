#import "mod.typ": *

#show: book-page.with(title: "Syntax")

// #set page(height: auto)

#show raw.where(lang: "example", block: true): set block(above: 0.5em)

= #text(1.4em)[Cosmo Syntax BNF Specification]

#let divider = context line(length: 109% - 2.5cm / 2, stroke: 0.5pt + text.fill)
#let part(content) = [
  // #pagebreak()
  = #content
  #divider
]

Principles:

- Minimal Syntax Base
- Composition-first polymorphism.
- Consistent syntax.
- Modular design and C++ interoperability.

#part[ Part 0: Notation]

The syntax is specified using a variant of Extended Backus-Naur Form (EBNF):

```scala
Syntax      = { Production } .
Production  = ProdName "=" [ Expression ] "." .
Expression  = Term { "|" Term } .
Term        = Factor { Factor } .
Factor      = ProdName | Token [ "…" Token ] | Group | Option | Repetition .
Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```

Productions are expressions constructed from terms and the following operators, in increasing precedence:

```
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```

Lowercase production names are used to identify lexical (terminal) tokens. Non-terminals are in CamelCase. Lexical tokens are enclosed in double quotes "" or back quotes ``.

The form a … b represents the set of characters from a through b as alternatives. The horizontal ellipsis … is also used elsewhere in the spec to informally denote various enumerations or code snippets that are not further specified. The character … (as opposed to the three characters ...) is not a token of the Go language.

#part[ Part 1: Literal]

== Identifier

```scala
Identifier = XIDStart { XIDContinue }
```

```example
name, _name, name1
```

== Integer Literals

```scala
IntLit = DecimalLit | BinaryLit | OctalLit | HexLit
```

```example
0, 0b0, 0o0, 0x0
```

== Floating-point Literals

```scala
FloatLit = DecimalFloatLit | HexFloatLit
```

```example
0., 1., 1e2, 0x1p-2
```

== String Literals

```scala
StringLit = `"` { UnicodeChar | ByteValue } `"` |
 `"""` { UnicodeChar | NewLine } `"""` |
 `""""` { UnicodeChar | NewLine } `""""` | ...
```

```example
"", "a", "a\x00", """a"""
```

== Template Literals

```scala
TemplateLiterals = IdentifierPath StringLitWithInterpolation
IdentifierPath = Identifier { "." Identifier }
```

String lit can contain `Interpolation` and intepolation MUSTn't contain nested string literals.

```scala
Interpolation = "${" Expression (":" Formatter) "}"
Formatter = { AnyCharOtherThanBrace }
```

```example
s"${name} is ${age} years old"
s"price is ${price:0.2f}"
```

== Char Literals

Creating a character literal by template with `c` prefix.

```scala
CharLiterals = "c" StringLit
```

```example
c"a", c"\x00"
```

== Bytes Literals

Creating a bytes literal by template with `b` prefix.

```scala
BytesLiterals = "b" StringLit
```

```example
b"a", b"\x00"
```

== Constant Literals

```scala
BoolLit = "true" | "false"
```

== Argument Like

```scala
ArgsLike = "(" ")" | ("(" ArgLike { "," ArgLike } [","] ")")
ArgLike = Expression | ".." ArgsLike | (ArgsLike [ ":" Expression ] [ "=" Expression ])
```

== Argument Clause

`Args` is subset of `ArgsLike` with only allowing following syntax:

```scala
Args = "(" ")" | ("(" Arg { "," Arg } [","] ")")
Arg = Expression | Destructuring | ".." Args | (Args [ ":" Type ] [ "=" Expression ])
```

== Array Literals

`ArrayLit` is subset of `ArgsLike` with only allowing following syntax:

```scala
ArrayLit = "(" ")" | ("(" Expression { "," Expression } [","] ")")
```

```example
(), (1,), (1, 2, 3)
```

== Dict Literals

`DictLit` is subset of `ArgsLike` with only allowing following syntax:

```scala
DictLit = "(" DictEntry { "," DictEntry } [","] ")"
DictEntry = IdentifierPath ":" Expression
```

```example
(), (a: 1), (a: 1, b: 2)
```

== Lambda Literal

```scala
LambdaLit = (Identifier | Args) "=>" Expression
```

#part[ Part 2: Declarations]

== Declarations

```scala
Declarations = VarDecl | ClassDecl | DefDecl
```

== Destructuring Clause

`Destructuring` is subset of `ArgsLike` with only allowing following syntax:

```scala
Destructuring = ("(" DestructureEntry { "," DestructureEntry } [","] ")")
DestructureEntry = Identifier | (IdentifierPath ":" Destructuring) | Destructuring
```

== Var/Val/Type Declaration

```scala
VarDecl = VarDeclHeader ["=" Expression]
VarDeclHeader = VarKeywords (Identifier | Destructuring) [ ":" Type ]
VarKeywords = "val" | "var" | "type"
```

```example
val name = "cosmo"
var age = 18
type Age = int
val (a, b) = (1, 2)
val (a: b) = (a: 1)
```

== Class/Trait Declaration

```scala
ClassDecl = ClassKeywords [Identifier] [Args] Block
ClassKeywords = "class" | "trait"
```

== Def (Function) Declaration

```scala
DefDecl = "def" Identifier [Args] [ ":" Type ] ["=" Expression]
```

#part[ Part 3: Expressions]

types and expressions shares same expression syntax.

```scala
Expression = Type = Expressions | Declarations | Literals
```

== Binary Expressions

```scala
BinaryExpr = Expression BinaryOp Expression
BinaryOp = "+" | "-" | "*" | "/" | "%" | "and" | "or" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "<<" | ">>" | "&" | "|" | "^"
```

Pattern matching is a special binary expression:

```scala
PatternMatch = Expression "match" CaseBlock
CaseBlock = "{" { CaseClause } "}"
CaseClause = "case" Args [ "if" Expression ] "=>" Expression
```

Cast is a special binary expression:

```scala
CastExpr = Expression "as" Type
```

Range is a special binary expression:

```scala
RangeExpr = Expression ".." Expression
```

== Unary Expressions

```scala
UnaryExpr = PrefixUnaryOp Expression | Expression PostfixUnaryOp
PrefixUnaryOp = "+" | "-" | "not" | "~"
PostfixUnaryOp = "?" | "!"
```

== Term Expressions

```scala
TermExpr = Identifier | Expression "." Identifier | CallExpr | "(" Expression ")"
```

== Call (Apply) Expressions

```scala
CallExpr = Expression Args
ForComprehension = CallExpr [Block]
```

== If Expression

```scala
IfStmt = "if" "(" Expression ")" Block { ElseIfStmt } [ "else" Block ]
ElseIfStmt = "else" "if" "(" Expression ")" Block
```

== Block Expression

```scala
Block = [ Tag ] "{" { Statements | CaseClause } "}"
Tag = "'" Identifier
```

== Decorated (Macro) Expression

functions, including macros, can be used for decorating syntax structures.

```scala
DecoratedExpr = { "@" CallExpr "\n" } (Declarations | Statements))
```

Example:

```scala
@nomangle
def add(a: int, b: int) = a + b
```

```scala
class A {
  @json("theName")
  val the_name = "cosmo"
}
```

#part[ Part 4: Statements]

Statements cannot be used as expressions.

```scala
Statements = Statement { (";" | newline) Statement [";"] }
Statement = Expression | Declarations | ...
```

== Expression Statement

```scala
ExprStmt = Expression ";"
```

== Import Statement

```scala
ImportStmt = ImportPathStmt | ImportBindStmt
ImportPathStmt = "import" IdentifierPath
ImportBindStmt = "import" ImportDest from Expression
ImportDest = Identifier ["," Destructuring]
```

== Infinite Loop Statement

```scala
ForInfStmt = [ Tag ] "for" Block
```

== While Loop Statement

```scala
ForUntilStmt = [ Tag ] "for" "(" Expression ")" Block
```

== Range Loop Statement

```scala
ForRangeStmt = [ Tag ] "for" [ForClause] Block
ForClause = "(" [ VarDeclHeader "in" Expression ] ")"
```

== Break/Continue/Return Statement

```scala
BreakContinueStmt = ("break" | "continue") [ Tag ] [ Expression ]
ReturnStmt = "return" [ Expression ]
```

== Pub Statement

Which MUST occur in generated header.

```scala
PubStmt = "pub" Declaration
```

#part[ Part 5: Macros]

`DeerivedMacro` can only generate extra syntax structures.

```scala
Macro = CallMacro | BlockMacro | DeerivedMacro
CallMacro = CallExpr ArgLike
BlockMacro = CallExpr Block
DeerivedMacro = DecoratedExpr
```

```example
@derive(Debug)
class A {
  val name = "cosmo"
}
```

```example
val (Q, R) = decompositeMatrix(M) ( // format: 2c
  Q, 0,
  0, R,
)
val I_4 = matrix(4, 4) (
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1,
)
```

```example
shell {
  for (val i in 0..10) {
    exec "mkdir $i"
  }
}
```

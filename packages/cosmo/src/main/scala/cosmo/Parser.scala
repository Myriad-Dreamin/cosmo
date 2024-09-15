package cosmo

import scala.util.chaining._
import scala.util.TupledFunction
import fastparse._, fastparse.ScalaWhitespace._

import cosmo.syntax._

def jt(x: Unit) = true
def us(x: Node) = x match {
  case Semi(Some(x)) => x
  case x             => x
}

object Parser {
  // Entry point
  def root[$: P]: P[Block] =
    P(("" ~ term).rep.map(_.toList) ~ End).map(Block.apply)
  // Lexer
  def keyword[$: P](s: String) = s ~~ !(letter | digit | "_" | "'")
  def letter[$: P] = P(lowercase | uppercase)
  def lowercase[$: P] = P(CharIn("a-z"))
  def uppercase[$: P] = P(CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def ident[$: P]: P[String] =
    P((letter | "_") ~~ (letter | digit | "_" | "'").repX).!.filter(
      !keywords(_),
    )
  def booleanLit[$: P] =
    (P(keyword("true") | keyword("false"))).!.map(v => BoolLit(v == "true"))
  def numberLit[$: P] = P(
    floatnumber.map(FloatLit.apply) |
      integer.map(IntLit.apply),
  )
  def todoLit[$: P] = P("???").map(_ => TodoLit)

  def stringLit[$: P]: P[StringLit] =
    P(longstring | shortstring).map(StringLit.apply)
  def shortstring[$: P]: P[String] = P(shortstring0("\"").map(unescapeStr))
  def shortstring0[$: P](delimiter: String) = P(
    delimiter ~~/ (!"\"" ~~ shortstringitem).repX.! ~~ delimiter,
  )
  def longstring[$: P]: P[String] = P(
    "\"".repX(3).!./.flatMapX { delimiter =>
      if delimiter.length() % 2 == 1 then longstring0(delimiter)
      else P("".map(_ => ""))
    },
  )
  def longstring0[$: P](delimiter: String) = P(
    (!delimiter ~~ longstringitem(delimiter)).repX.! ~~ delimiter,
  )
  def shortstringitem[$: P]: P[Unit] = P(litCharsWhile("\\\n\"") | escapeseq)
  def longstringitem[$: P](delimiter: String): P[Unit] = P(
    lStringChars("\"", delimiter).repX.!,
  )

  def tmplLit[$: P] =
    P(tmplPath ~ &("\"") ~ tmplLitParts).map(TmplApply.apply.tupled)
  def tmplPath[$: P] = P(
    ident.filter(_ != "from").map(Ident.apply) ~ (P(
      ("." | "::") ~ identifier,
    )).rep,
  ).map { case (lhs, rhs) =>
    rhs.foldLeft(lhs: Node)((a, b) => Select(a, b, true))
  }
  def tmplLitParts[$: P] = P(longTmplLit | shortTmplLit)
  def shortTmplLit[$: P]: P[List[(String, Option[(Node, Option[String])])]] = P(
    "\"" ~~/ (!"\"" ~~ shortTmplLitItem).repX ~~ "\"",
  ).map(_.toList)
  def longTmplLit[$: P]: P[List[(String, Option[(Node, Option[String])])]] = P(
    "\"\"\"" ~~/ (!"\"\"\"" ~~ longTmplLitItem("\"\"\"")).repX ~~ "\"\"\"",
  ).map(_.toList)
  def shortTmplLitItem[$: P]: P[(String, Option[(Node, Option[String])])] = P(
    (litCharsWhile("\\\n\"$") | escapeseq).repX.! ~~/ tmplExpr,
  )
  def longTmplLitItem[$: P](delimiter: String) = P(
    lStringChars("\"$", delimiter).repX.! ~~/ tmplExpr,
  )

  def lStringChars[$: P](mores: String, delimiter: String): P[Unit] = P(
    litCharsWhile(mores) | !delimiter ~~ delimiter.take(1),
  )
  def litCharsWhile[$: P](mores: String) = P(
    CharsWhile(!mores.contains(_)),
  )
  def escapeseq[$: P]: P[Unit] = P("\\" ~ AnyChar)
  def tmplExpr[$: P]: P[Option[(Node, Option[String])]] = P(
    &("\"").map(_ => None) | ("$" ~~/ P(
      "{" ~/ compound ~ tmplAnnotation.? ~ "}",
    ).?),
  )
  def tmplAnnotation[$: P]: P[String] = P(
    ":" ~/ CharsWhile(!s"}\"".contains(_)).!,
  )

  def negatable[T, $: P](p: => P[T])(implicit ev: Numeric[T]) =
    (("+" | "-").?.! ~ p).map {
      case ("-", i) => ev.negate(i)
      case (_, i)   => i
    }

  def integer[$: P]: P[BigInt] = negatable[BigInt, Any](
    P(octinteger | hexinteger | bininteger | decimalinteger),
  )
  def decimalinteger[$: P]: P[BigInt] =
    P(nonzerodigit ~ digit.rep | "0").!.map(scala.BigInt(_))
  def octinteger[$: P]: P[BigInt] = P(
    "0" ~ ("o" | "O") ~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).!,
  ).map(scala.BigInt(_, 8))
  def hexinteger[$: P]: P[BigInt] =
    P("0" ~ ("x" | "X") ~ hexdigit.rep(1).!).map(scala.BigInt(_, 16))
  def bininteger[$: P]: P[BigInt] =
    P("0" ~ ("b" | "B") ~ bindigit.rep(1).!).map(scala.BigInt(_, 2))
  def nonzerodigit[$: P]: P[Unit] = P(CharIn("1-9"))
  def octdigit[$: P]: P[Unit] = P(CharIn("0-7"))
  def bindigit[$: P]: P[Unit] = P("0" | "1")
  def hexdigit[$: P]: P[Unit] = P(digit | CharIn("a-f", "A-F"))

  def floatnumber[$: P]: P[BigDecimal] =
    negatable[BigDecimal, Any](P(pointfloat | exponentfloat))
  def pointfloat[$: P]: P[BigDecimal] =
    P(intpart.? ~ fraction | intpart ~ "." ~~ !".").!.map(BigDecimal(_))
  def exponentfloat[$: P]: P[BigDecimal] =
    P((intpart | pointfloat) ~ exponent).!.map(BigDecimal(_))
  def intpart[$: P]: P[BigDecimal] = P(digit.rep(1)).!.map(BigDecimal(_))
  def fraction[$: P]: P[Unit] = P("." ~ digit.rep(1))
  def exponent[$: P]: P[Unit] = P(("e" | "E") ~ ("+" | "-").? ~ digit.rep(1))

  // Terms
  def term[$: P]: P[Node] = P(
    decorator.? ~ decorated,
  ).map { case (dr, de) => dr.map(Decorate(_, de)).getOrElse(de) }
  def termU[$: P] = P(term.map(us))
  def decorator[$: P]: P[Node] = P("@" ~/ factor)
  def decorated[$: P] =
    ifItem | forItem | whileItem | loopItem | caseClause | semiWrap | semi
  def semiWrap[$: P]: P[Node] = P(
    P(
      canExportItem | implItem | breakItem | continueItem | returnItem | compound,
    ) ~/ semi.?,
  ).map((item, isSemi) => if isSemi.isEmpty then item else Semi(Some(item)))
  def semi[$: P] = P(";").map(_ => Semi(None))
  def canExportItem[$: P]: P[Node] =
    (keyword("pub") | keyword("private")).? ~ P(
      defItem | valItem | varItem | classItems | importItem,
    )
  def primaryExpr[$: P] = P(tmplLit | identifier | literal | parens | braces)
  def factor[$: P] = P(unary | factorBase)
  def factorBase[$: P]: P[Node] = P(
    primaryExpr ~ (
      P(".".map(_ => ".") ~ identifier) |
        P("::".map(_ => ":") ~ identifier) |
        args.map(a => ("(", a))
    ).rep,
  ).map { case (lhs, parts) =>
    parts.foldLeft(lhs) { case (lhs, (op, rhs)) =>
      op match {
        case "." => Select(lhs, rhs.asInstanceOf[Ident], false)
        case ":" => Select(lhs, rhs.asInstanceOf[Ident], true)
        case "(" => Apply(lhs, rhs.asInstanceOf[Seq[Node]].toList)
      }
    }
  }

  // Expressions
  def literal[$: P] = P(numberLit | booleanLit | stringLit | todoLit)
  def identifier[$: P] = ident.map(Ident.apply)
  def defItem[$: P] = P(sigItem("def") ~ typeAnnotation.? ~ initExpression.?)
    .map(Def.apply.tupled)
  def classItems[$: P] = classItem("class", false) | classItem("trait", true)
  def classItem[$: P](kw: String, abc: Boolean) =
    P(sigItem(kw) ~ termU ~ "".map(_ => abc)).map(Class.apply.tupled)
  def implItem[$: P] = P(
    keyword("impl") ~ params ~/ factor ~/ (keyword("for") ~/ factor).? ~ braces,
  ).map {
    case (params, lhs, Some(rhs), body) => Impl(rhs, Some(lhs), params, body)
    case (params, lhs, None, body)      => Impl(lhs, None, params, body)
  }
  def sigItem[$: P](kw: String) = P(keyword(kw) ~/ ident ~ params)
  def valItem[$: P] = P(varLike("val")).map(Val.apply.tupled)
  def varItem[$: P] = P(varLike("var")).map(Var.apply.tupled)
  def typeItem[$: P] = P(varLike("type")).map(Typ.apply.tupled)
  def varLike[$: P](kw: String) =
    P(keyword(kw) ~/ ident ~ typeAnnotation.? ~ initExpression.?)
  def loopItem[$: P] = P(keyword("loop") ~/ braces).map(Loop.apply)
  def importItem[$: P] =
    P(keyword("import") ~/ termU ~ (keyword("from") ~/ termU).?).map {
      case (dest, Some(Semi(Some(path)))) =>
        Semi(Some(Import(path, Some(dest))))
      case (Semi(Some(path)), None) => Semi(Some(Import(path, None)))
      case (dest, Some(path))       => Import(path, Some(dest))
      case (path, None)             => Import(path, None)
    }
  def forItem[$: P] = P(
    keyword("for") ~ "(" ~/ ident ~ keyword("in") ~ term ~ ")" ~ braces,
  ).map(For.apply.tupled)
  def whileItem[$: P] = P(
    keyword("while") ~/ parens ~ braces,
  ).map(While.apply)
  def breakItem[$: P] = P(keyword("break")).map(_ => Break())
  def continueItem[$: P] = P(keyword("continue")).map(_ => Continue())
  def ifItem[$: P]: P[Node] = P(
    keyword("if") ~/ parens ~ braces
      ~ (keyword("else") ~ P(ifItem | braces)).?,
  ).map(If.apply.tupled)
  def returnItem[$: P] = P(keyword("return") ~/ termU).map(Return.apply)
  // Compound expressions
  def compound[$: P] = P(
    assign ~ (
      P(keyword("match").map(_ => "m") ~/ braces) |
        P(keyword("as").map(_ => "a") ~/ factor)
    ).rep,
  ).map { case (lhs, rhs) =>
    rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
      op match {
        case "m" => Match(lhs, rhs)
        case "a" => As(lhs, rhs)
      }
    }
  }
  def moreAssigns[$: P] =
    "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
  def assign[$: P]: P[Node] = binOp(P(P("=" ~ !">") | moreAssigns), andOr)
  def binOp[$: P](op: => P[Unit], next: => P[Node]): P[Node] =
    P(next ~ (op.! ~/ next).rep).map { case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
        BinOp(op, lhs, rhs)
      }
    }
  def andOr[$: P]: P[Node] = binOp(P("and" | "or"), compare)
  def relOp[$: P] = "<=" | ">=" | "==" | "!=" | P("<" ~ !"<") | P(">" ~ !">")
  def relTypeOp[$: P] = "<:" | ">:" | "=:"
  def inNotIn[$: P] = keyword("in") | (keyword("not") ~ keyword("in"))
  def compare[$: P]: P[Node] = binOp(P(relTypeOp | relOp | inNotIn), range_lit)
  def range_lit[$: P]: P[Node] = binOp(P(".."), bitOr)
  def bitOr[$: P]: P[Node] = binOp(CharIn("|") ~~ !"=", bitAnd)
  def bitAnd[$: P]: P[Node] = binOp(CharIn("&") ~~ !"=", bitShift)
  def bitShift[$: P]: P[Node] = binOp(P(("<<" | ">>") ~~ !"="), addSub)
  def addSub[$: P]: P[Node] = binOp(CharIn("+\\-") ~~ !"=", divMul)
  def divMul[$: P]: P[Node] = binOp(CharIn("*/") ~~ !"=", arithMod)
  def arithMod[$: P]: P[Node] = binOp(CharIn("%") ~~ !"=", factor)
  def unary[$: P]: P[Node] =
    P(P("!" | "~" | "-" | "+" | "&" | keyword("mut")).! ~ factor)
      .map(UnOp.apply.tupled)
  def spread[$: P]: P[Node] = P(".." ~/ arg).map(UnOp("..", _))
  // Clauses
  def parens[$: P] = P(
    "(" ~/ arg.rep(sep = ",") ~ ("," ~ &(")")).map(jt).? ~ ")",
  ).map(p => {
    if p._2.isEmpty && p._1.size == 1 && !p._1.head.isInstanceOf[KeyedArg] then
      p._1.head
    else ArgsLit(p._1.toList)
  })
  def braces[$: P]: P[Node] =
    P("{" ~/ term.rep.map(_.toList) ~ "}").map(body => {
      // check if all terms are cases
      var caseItems = List.empty[Case]
      var anyNotCase = false
      body.foreach {
        case c: Case => caseItems = caseItems :+ c
        case _       => anyNotCase = true
      }
      if anyNotCase || body.isEmpty then Block(body)
      else CaseBlock(caseItems)
    })
  def args[$: P] = P("(" ~/ arg.rep(sep = ",") ~ ")")
  def arg[$: P] = P(spread | keyedClause)
  def params[$: P] =
    (P(paramsCt.map(_.flatten) | paramsRt).rep).map { seq =>
      if seq.isEmpty then None else Some(seq.flatten.toList)
    }
  def paramsCt[$: P] =
    P("[" ~/ P(introTy | constraint.map(e => Seq(e))).rep(sep = ",") ~ "]")
  def paramsRt[$: P] =
    P("(" ~/ P((selfSugar | param).rep(sep = ",")) ~ ")")
  def introTy[$: P] =
    P(ident.rep(1, sep = " ") ~ typeAnnotation.? ~ &("," | "]")).map((e, t) =>
      e.map(Param(_, t.orElse(Some(Ident("Type"))), None, true)),
    )
  def constraint[$: P] = compare.map(e => Param("_", Some(e), None, true))
  def selfSugar[$: P] = P(
    "&".?.map(jt) ~ keyword("mut").?.map(jt) ~ keyword("self") ~ &("," | ")"),
  ).map((isRef, isMut) => {
    val ty1 = Ident("Self")
    val decorated = (isRef, isMut) match {
      case (true, true)  => Apply(Ident("RefMut"), List(ty1))
      case (true, false) => Apply(Ident("Ref"), List(ty1))
      case _             => ty1
    }
    Param("self", Some(decorated), None, false)
  })
  def param[$: P] =
    P((ident) ~ typeAnnotation.? ~ initExpression.? ~ "".map(_ => false))
      .map(Param.apply.tupled)
  def typeAnnotation[$: P] = P(":" ~/ factor)
  def initExpression[$: P] = P("=" ~/ term)
  def matchClause[$: P] = P(keyword("match") ~/ braces)
  def caseClause[$: P] =
    P("case" ~/ termU ~ ("=>" ~ term).?).map(Case.apply.tupled)
  def keyedClause[$: P] = P(
    (compound ~ (":" ~/ compound).?)
      .map { case (lhs, rhs) =>
        rhs match {
          case Some(rhs) => KeyedArg(lhs, rhs)
          case None      => lhs
        }
      },
  )

  // Keywords
  val keywords =
    Set(
      "pub",
      "private",
      "impl",
      "yield",
      "lazy",
      "as",
      "import",
      "module",
      "unsafe",
      "match",
      "implicit",
      "break",
      "continue",
      "using",
      "throw",
      "return",
      "case",
      "def",
      "class",
      "trait",
      "type",
      "if",
      "else",
      "for",
      "while",
      "loop",
      "val",
      "var",
      "and",
      "or",
      "in",
      "not",
      "mut",
      "true",
      "false",
      // weak
      // "from",
    )
}

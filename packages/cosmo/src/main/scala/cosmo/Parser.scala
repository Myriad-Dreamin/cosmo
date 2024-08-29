package cosmo

import scala.util.chaining._
import scala.util.{TupledFunction, Left, Right, Either}
import fastparse._, fastparse.ScalaWhitespace._

import cosmo.syntax._

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
  def selfLit[$: P] =
    (keyword("self").map(_ => Self) | keyword("Self").map(_ => BigSelf))
  def booleanLit[$: P] =
    (P(keyword("true") | keyword("false"))).!.map(v => BoolLit(v == "true"))
  def numberLit[$: P] = P(digit.rep(1).!.map(_.toInt).map(IntLit.apply))
  def todoLit[$: P] = P("???").map(_ => TodoLit)

  def stringLit[$: P]: P[StringLit] =
    P(longstring | shortstring).map(StringLit.apply)
  def shortstring[$: P]: P[String] = P(shortstring0("\""))
  def shortstring0[$: P](delimiter: String) = P(
    delimiter ~~/ (!"\"" ~~ shortstringitem).repX.! ~~ delimiter,
  )
  def longstring[$: P]: P[String] = P(longstring0("\"\"\""))
  def longstring0[$: P](delimiter: String) = P(
    delimiter ~~/ (!"\"\"\"" ~~ longstringitem).repX.! ~~ delimiter,
  )
  def shortstringitem[$: P]: P[Unit] = P(litCharsWhile("\\\n\"") | escapeseq)
  def longstringitem[$: P]: P[Unit] = P(lStringChars("\\\"").repX.!)

  def tmplLit[$: P] =
    P(tmplPath ~ &("\"") ~ tmplLitParts).map(TmplApply.apply.tupled)
  def tmplPath[$: P] = P(
    (ident.filter(_ != "from") | selfIdent).map(Ident.apply) ~ (P(
      "." ~ identifier,
    )).rep,
  ).map { case (lhs, rhs) => rhs.foldLeft(lhs: Node)(Select.apply) }
  def tmplLitParts[$: P] = P(longTmplLit | shortTmplLit)
  def shortTmplLit[$: P]: P[List[(String, Option[(Node, Option[String])])]] = P(
    "\"" ~~/ (!"\"" ~~ shortTmplLitItem).repX ~~ "\"",
  ).map(_.toList)
  def longTmplLit[$: P]: P[List[(String, Option[(Node, Option[String])])]] = P(
    "\"\"\"" ~~/ (!"\"\"\"" ~~ longTmplLitItem).repX ~~ "\"\"\"",
  ).map(_.toList)
  def shortTmplLitItem[$: P]: P[(String, Option[(Node, Option[String])])] = P(
    (litCharsWhile("\n\"$") | escapeseq).repX.! ~~/ tmplExpr,
  )
  def longTmplLitItem[$: P] = P(lStringChars("\\\"$").repX.! ~~/ tmplExpr)

  def lStringChars[$: P](mores: String): P[Unit] = P(
    litCharsWhile(mores) | escapeseq | !"\"\"\"" ~~ "\"\"\"".take(1),
  )
  def litCharsWhile[$: P](mores: String) = P(
    CharsWhile(!s"\\${mores}".contains(_)),
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

  // Terms
  def term[$: P]: P[Node] = P(
    ifItem | forItem | loopItem | caseClause | semiWrap | semi,
  )
  def semiWrap[$: P]: P[Node] = P(
    P(
      canExportItem | breakItem | continueItem | returnItem | compound,
    ) ~/ semi.?,
  ).map((item, isSemi) => if isSemi.isEmpty then item else Semi(Some(item)))
  def semi[$: P] = P(";").map(_ => Semi(None))
  def canExportItem[$: P]: P[Node] =
    (keyword("pub") | keyword("private")).? ~ P(
      defItem | valItem | varItem | classItem | importItem,
    )
  def primaryExpr[$: P] = P(tmplLit | identifier | literal | parens | braces)
  def factor[$: P]: P[Node] = P(
    primaryExpr ~ (P("." ~ identifier).map(Left.apply) | args.map(
      Right.apply,
    )).rep,
  ).map { case (lhs, parts) =>
    parts.foldLeft(lhs) { case (lhs, part) =>
      part match {
        case Left(rhs)   => Select(lhs, rhs)
        case Right(args) => Apply(lhs, args.toList)
      }
    }
  }
  // Expressions
  def literal[$: P] = P(numberLit | selfLit | booleanLit | stringLit | todoLit)
  def identifier[$: P] = ident.map(Ident.apply)
  def defItem[$: P] = P(sigItem("def") ~ typeAnnotation.? ~ initExpression.?)
    .map(Def.apply.tupled)
  def classItem[$: P] = P(sigItem("class") ~ term).map(Class.apply.tupled)
  def sigItem[$: P](kw: String) = P(keyword(kw) ~/ ident ~ params.?)
  def valItem[$: P] =
    P(keyword("val") ~/ ident ~ typeAnnotation.? ~ initExpression.?)
      .map(Val.apply.tupled)
  def varItem[$: P] =
    P(keyword("var") ~/ ident ~ typeAnnotation.? ~ initExpression.?)
      .map(Var.apply.tupled)
  def loopItem[$: P] = P(keyword("loop") ~/ braces).map(Loop.apply)
  def importItem[$: P] =
    P(keyword("import") ~/ term ~ (keyword("from") ~/ term).?).map {
      case (dest, Some(Semi(Some(path)))) =>
        Semi(Some(Import(path, Some(dest))))
      case (Semi(Some(path)), None) => Semi(Some(Import(path, None)))
      case (dest, Some(path))       => Import(path, Some(dest))
      case (path, None)             => Import(path, None)
    }
  def forItem[$: P] = P(
    keyword("for") ~ "(" ~/ ident ~ keyword("in") ~ term ~ ")" ~ braces,
  ).map(For.apply.tupled)
  def breakItem[$: P] = P(keyword("break")).map(_ => Break())
  def continueItem[$: P] = P(keyword("continue")).map(_ => Continue())
  def ifItem[$: P]: P[Node] = P(
    keyword("if") ~/ parens ~ braces
      ~ (keyword("else") ~ P(ifItem | braces)).?,
  ).map(If.apply.tupled)
  def returnItem[$: P] = P(keyword("return") ~/ term).map(Return.apply)
  // Compound expressions
  def compound[$: P] = P(
    addSub ~ (
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
  def addSub[$: P] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
    case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
        BinOp(op, lhs, rhs)
      }
  }
  def divMul[$: P] = P(compare ~ (CharIn("*/").! ~/ compare).rep).map {
    case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
        BinOp(op, lhs, rhs)
      }
  }
  def compare[$: P] = P(
    range_lit ~ (P(
      "<=" | ">=" | "==" | "!=" | "<" | ">",
    ).! ~/ range_lit).rep,
  ).map { case (lhs, rhs) =>
    rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
      BinOp(op, lhs, rhs)
    }
  }
  def range_lit[$: P] = P(
    assign ~ (P(
      "..",
    ).! ~/ assign).rep,
  ).map { case (lhs, rhs) =>
    rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
      BinOp(op, lhs, rhs)
    }
  }
  def assign[$: P] = P(
    factor ~ (P(
      ("=" ~ !">") | "+=" | "-=" | "*=" | "/=",
    ).! ~/ factor).rep,
  ).map { case (lhs, rhs) =>
    rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
      BinOp(op, lhs, rhs)
    }
  }
  // Clauses
  def parens[$: P] = P("(" ~/ term ~ ")")
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
  def arg[$: P] = P(keyedClause | compound)
  def params[$: P] = P("(" ~/ P(param.rep(sep = ",")) ~ ")").map(_.toList)
  def param[$: P] =
    P((ident | selfIdent) ~ typeAnnotation.? ~ initExpression.?)
      .map(Param.apply.tupled)
  def selfIdent[$: P] = keyword("self").map(_ => "self")
  def typeAnnotation[$: P] = P(":" ~/ factor)
  def initExpression[$: P] = P("=" ~/ term)
  def matchClause[$: P] = P(keyword("match") ~/ braces)
  def caseClause[$: P] =
    P("case" ~/ term ~ ("=>" ~ term).?).map(Case.apply.tupled)
  def keyedClause[$: P] = P(
    ((ident | selfIdent) ~ (":" | "=" | "->") ~/ compound)
      .map(KeyedArg.apply.tupled),
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
      "self",
      "class",
      "trait",
      "type",
      "if",
      "else",
      "for",
      "loop",
      "val",
      "var",
      "and",
      "or",
      "in",
      "not",
      "true",
      "false",
      "none",
      // weak
      // "from",
    )
}

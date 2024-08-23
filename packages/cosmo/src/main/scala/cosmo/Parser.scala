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
  def number[$: P]: P[Int] = P(digit.rep(1).!.map(_.toInt))
  def ident[$: P]: P[String] =
    P((letter | "_") ~~ (letter | digit | "_" | "'").repX).!.filter(
      !keywords(_),
    )
  def stringLit[$: P]: P[String] = P(P("s").? ~ (longstring | shortstring))
  def shortstring[$: P]: P[String] = P(shortstring0("\""))
  def shortstring0[$: P](delimiter: String) = P(
    delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter,
  )
  def shortstringitem[$: P](quote: String): P[Unit] = P(
    shortstringchar(quote) | escapeseq,
  )
  def shortstringchar[$: P](quote: String): P[Unit] = P(
    CharsWhile(!s"\\\n${quote(0)}".contains(_)),
  )
  def longstring[$: P]: P[String] = P(longstring0("\"\"\""))
  def longstring0[$: P](delimiter: String) = P(
    delimiter ~ longstringitem(delimiter).rep.! ~ delimiter,
  )
  def longstringitem[$: P](quote: String): P[Unit] = P(
    longstringchar(quote) | escapeseq | !quote ~ quote.take(1),
  )
  def longstringchar[$: P](quote: String): P[Unit] = P(
    CharsWhile(!s"\\${quote(0)}".contains(_)),
  )
  def escapeseq[$: P]: P[Unit] = P("\\" ~ AnyChar)

  // Terms
  def term[$: P]: P[Node] = P(
    canExportItem | ifItem | forItem | breakItem | continueItem | returnItem | loopItem | caseClause | compound,
  )
  def canExportItem[$: P]: P[Node] =
    keyword("pub").? ~ P(defItem | valItem | varItem | classItem | importItem)
  def primaryExpr[$: P] = P(identifier | literal | parens | braces)
  def factor[$: P]: P[Node] = P(
    primaryExpr ~ (P("." ~ identifier).map(Left.apply) | P(
      "(" ~/ term.rep(sep = ",") ~ ")",
    ).map(Right.apply)).rep,
  ).map { case (lhs, parts) =>
    parts.foldLeft(lhs) { case (lhs, part) =>
      part match {
        case Left(rhs)   => Select(lhs, rhs)
        case Right(args) => Apply(lhs, args.toList)
      }
    }
  }
  // Expressions
  def literal[$: P] = number.map(Literal.apply)
  def identifier[$: P] = ident.map(Ident.apply)
  def defItem[$: P] =
    P(keyword("def") ~/ ident ~ ("(" ~/ params ~ ")").? ~ "=" ~ term)
      .map(Def.apply.tupled)
  def valItem[$: P] =
    P(keyword("val") ~/ ident ~ typeAnnotation.? ~ initExpression.?)
      .map(Val.apply.tupled)
  def varItem[$: P] =
    P(keyword("var") ~/ ident ~ typeAnnotation.? ~ initExpression.?)
      .map(Var.apply.tupled)
  def loopItem[$: P] = P(keyword("loop") ~/ braces).map(Loop.apply)
  def importItem[$: P] = P(keyword("import") ~/ stringLit).map(Import.apply)
  def forItem[$: P] = P(
    keyword("for") ~ "(" ~/ ident ~ keyword("in") ~ term ~ ")" ~ braces,
  ).map(For.apply.tupled)
  def breakItem[$: P] = P(keyword("break")).map(_ => Break())
  def continueItem[$: P] = P(keyword("continue")).map(_ => Continue())
  def ifItem[$: P]: P[Node] = P(
    keyword("if") ~/ parens ~ braces
      ~ (keyword("else") ~ P(ifItem | braces)).?,
  ).map(If.apply.tupled)
  def classItem[$: P] =
    P(keyword("class") ~/ ident ~ braces).map(Class.apply.tupled)
  def returnItem[$: P] = P(keyword("return") ~/ term).map(Return.apply)
  // Compound expressions
  def compound[$: P] = P(addSub ~ matchClause.?).map {
    case (lhs, Some(rhs)) => Match(lhs, rhs)
    case (lhs, None)      => lhs
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
      "=" | "+=" | "-=" | "*=" | "/=",
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
  def params[$: P] = P(param.rep(sep = ",")).map(_.toList)
  def param[$: P] =
    P(ident ~ typeAnnotation.? ~ initExpression.?).map(Param.apply.tupled)
  def typeAnnotation[$: P] = P(":" ~/ ident).map(Ident.apply)
  def initExpression[$: P] = P("=" ~/ term)
  def matchClause[$: P] = P(keyword("match") ~/ braces)
  def caseClause[$: P] =
    P("case" ~/ term ~ ("=>" ~ term).?).map(Case.apply.tupled)

  // Keywords
  val keywords =
    Set(
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
    )
}

package cosmo

import scala.util.chaining._
import scala.util.TupledFunction
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
  // Terms
  def term[$: P]: P[Node] = P(
    defItem | valItem | varItem | classItem | ifItem | loopItem | breakItem | continueItem
      | returnItem | caseClause | compound,
  )
  def factor[$: P]: P[Node] = P(
    applyItem | identifier | literal | parens | braces,
  )
  // Expressions
  def literal[$: P] = number.map(Literal.apply)
  def identifier[$: P] = ident.map(Ident.apply)
  def defItem[$: P] =
    P(keyword("def") ~/ ident ~ "(" ~/ params ~ ")" ~ "=" ~ term)
      .map(Def.apply.tupled)
  def valItem[$: P] =
    P(keyword("val") ~/ ident ~ typeAnnotation.? ~ initExpression.?)
      .map(Val.apply.tupled)
  def varItem[$: P] =
    P(keyword("var") ~/ ident ~ typeAnnotation.? ~ initExpression.?)
      .map(Var.apply.tupled)
  def loopItem[$: P] = P(keyword("loop") ~/ braces).map(Loop.apply)
  def breakItem[$: P] = P(keyword("break")).map(_ => Break())
  def continueItem[$: P] = P(keyword("continue")).map(_ => Continue())
  def ifItem[$: P]: P[Node] = P(
    keyword("if") ~/ parens ~ braces
      ~ (keyword("else") ~ P(ifItem | braces)).?,
  ).map(If.apply.tupled)
  def classItem[$: P] =
    P(keyword("class") ~/ ident ~ braces).map(Class.apply.tupled)
  def applyItem[$: P] = P(ident ~ "(" ~/ term.rep(sep = ",") ~ ")").map {
    case (name, args) => Apply(Ident(name), args.toList)
  }
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
    assign ~ (P(
      "<=" | ">=" | "==" | "!=" | "<" | ">",
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

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
    defItem | valItem | varItem | returnItem | addSub,
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
    P(keyword("val") ~/ ident ~ "=" ~ term).map(Val.apply.tupled)
  def varItem[$: P] =
    P(keyword("var") ~/ ident ~ "=" ~ term).map(Var.apply.tupled)
  def applyItem[$: P] = P(ident ~ "(" ~/ term.rep(sep = ",") ~ ")").map {
    case (name, args) => Apply(Ident(name), args.toList)
  }
  def returnItem[$: P] = P(keyword("return") ~/ term).map(Return.apply)
  // Arithmetics
  def addSub[$: P] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
    case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
        BinOp(op, lhs, rhs)
      }
  }
  def divMul[$: P] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map {
    case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) =>
        BinOp(op, lhs, rhs)
      }
  }
  // Clauses
  def parens[$: P] = P("(" ~/ term ~ ")")
  def braces[$: P] =
    P("{" ~/ term.rep.map(_.toList) ~ "}").map(Block.apply)
  def params[$: P]: P[List[Param]] =
    P(param.rep(sep = ",")).map(_.toList)
  def param[$: P]: P[Param] =
    P(ident ~ typeAnnotation.? ~ initExpression.?).map(Param.apply.tupled)
  def typeAnnotation[$: P]: P[Node] =
    P(":" ~/ ident).map(Ident.apply)
  def initExpression[$: P]: P[Node] =
    P("=" ~/ term)

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

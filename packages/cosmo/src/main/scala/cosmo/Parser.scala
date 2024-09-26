package cosmo

import scala.util.chaining._
import scala.util.TupledFunction
import fastparse._, fastparse.ScalaWhitespace._

import cosmo.syntax._
import cosmo.syntax.NodeParse._

implicit class SoftErr[$: P](p: => P[Unit]) {
  def err(msg: String): P[Unit] = p.map(_ => Err(msg))
}

object Parser {
  // Entry point
  def root[$: P]: P[Block] =
    P(("" ~ term).rep.map(_.toList) ~ End).map(Block.apply).m

  // Literals
  def booleanLit[$: P] =
    (P(word("true") | word("false"))).!.map(v => BoolLit(v == "true"))
  def numberLit[$: P] = P(float.map(FloatLit.apply) | int.map(IntLit.apply))
  def stringLit[$: P] = P(longStr | shortStr).map(StrLit.apply)
  def tmplLit[$: P] = P(tmplPath ~ &("\"") ~ tmplLitStr).map(TmplApply(_, _)).m
  def todoLit[$: P] = P("???").map(_ => TodoLit)

  // Lexer
  def word[$: P](s: String) = s ~~ !idCont
  def letter[$: P] = P(lowercase | uppercase)
  def lowercase[$: P] = P(CharIn("a-z"))
  def uppercase[$: P] = P(CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def id[$: P] = P((letter | "_") ~~ idCont.repX).!.filter(!keywords(_))
  def idCont[$: P] = P(letter | digit | "_")

  def delimStr[T, $: P](d: String, body: => P[T]) = d ~~/ delimStrCont(d, body)
  def delimStrCont[T, $: P](d: String, body: => P[T]) =
    (!End ~~ !d ~~ body).repX.! ~~ (d | End.err("Unclosed string"))
  def shortStr[$: P] = P(shortStr0.map(unescapeStr))
  def shortStr0[$: P] = delimStr("\"", strEscape)
  def longStr[$: P] = P("\"".repX(3).!./.flatMapX(longStr0))
  def longStr0[$: P](delim: String) = P(delimStrCont(delim, longStrBody(delim)))
  def strEscape[$: P] = P(litCharsWhile("\\\n\"") | escapeseq)
  def longStrBody[$: P](delim: String) = P(longChars("\"", delim).repX.!)

  def tmplPath[$: P] = P(
    id.filter(_ != "from").map(Ident.apply).m ~ (P(("." | "::") ~ ident)).rep,
  ).map { case (lhs, rhs) => rhs.foldLeft(lhs: Node)(Select(_, _, true)) }
  def tmplLitStr[$: P] = P(longTmplStr("\"\"\"") | shortTmplStr)
  def shortTmplStr[$: P] =
    P("\"" ~~/ (!"\"" ~~ shortTmplLitStr).repX ~~ "\"").map(_.toList)
  def longTmplStr[$: P](delim: String) =
    P(delim ~~/ (!delim ~~ longTmplStrBody(delim)).repX ~~ delim)
      .map(_.toList)
  def shortTmplLitStr[$: P]: P[(String, Option[(Node, Option[String])])] = P(
    (!"$" ~~ (litCharsWhile("\\\n\"$") | escapeseq)).repX.! ~~/ tmplExpr,
  )
  def longTmplStrBody[$: P](delim: String) =
    P((!"$" ~~ longChars("\"$", delim)).repX.! ~~/ tmplExpr)

  def longChars[$: P](mores: String, delim: String): P[Unit] =
    !End ~~ P(litCharsWhile(mores) | !delim ~~ "\"".repX)
  def litCharsWhile[$: P](mores: String) = P(CharsWhile(!mores.contains(_)))
  def escapeseq[$: P]: P[Unit] = P("\\" ~ AnyChar)
  def tmplExpr[$: P]: P[Option[(Node, Option[String])]] =
    P(&("\"").map(_ => None) | ("$" ~~/ P("{" ~/ tmplArg ~ "}").?))
  def tmplArg[$: P] = P(compound ~ (":" ~/ CharsWhile(!"}\"".contains(_)).!).?)

  def int[$: P]: P[BigInt] =
    P(octinteger | hexinteger | bininteger | decimalinteger)
  def decimalinteger[$: P]: P[BigInt] =
    P(nonzerodigit ~~ digit.rep | "0").!.map(scala.BigInt(_))
  def octinteger[$: P]: P[BigInt] = P(
    "0" ~ ("o" | "O") ~~ octdigit.rep(1).! | "0" ~ octdigit.rep(1).!,
  ).map(scala.BigInt(_, 8))
  def hexinteger[$: P]: P[BigInt] =
    P("0" ~ ("x" | "X") ~~ hexdigit.rep(1).!).map(scala.BigInt(_, 16))
  def bininteger[$: P]: P[BigInt] =
    P("0" ~ ("b" | "B") ~~ bindigit.rep(1).!).map(scala.BigInt(_, 2))
  def nonzerodigit[$: P]: P[Unit] = P(CharIn("1-9"))
  def octdigit[$: P]: P[Unit] = P(CharIn("0-7"))
  def bindigit[$: P]: P[Unit] = P("0" | "1")
  def hexdigit[$: P]: P[Unit] = P(digit | CharIn("a-f", "A-F"))

  def float[$: P]: P[BigDecimal] = P(pointfloat | exponentfloat)
  def pointfloat[$: P]: P[BigDecimal] =
    P(intpart.? ~~ fraction | intpart ~~ "." ~~ !".").!.map(BigDecimal(_))
  def exponentfloat[$: P]: P[BigDecimal] =
    P((intpart | pointfloat) ~~ exponent).!.map(BigDecimal(_))
  def intpart[$: P]: P[BigDecimal] = P(digit.rep(1)).!.map(BigDecimal(_))
  def fraction[$: P]: P[Unit] = P("." ~~ digit.rep(1))
  def exponent[$: P]: P[Unit] = P(("e" | "E") ~~ ("+" | "-").? ~ digit.rep(1))

  // Terms
  def term[$: P]: P[Node] = P(
    decorator.? ~ decorated,
  ).map { case (dr, de) => dr.map(Decorate(_, de)).getOrElse(de) }
  def termU[$: P] = P(term.map {
    case Semi(Some(x)) => x
    case x             => x
  })
  def decorator[$: P]: P[Node] = P("@" ~/ factor)
  def decorated[$: P] =
    (ifItem | forItem | whileItem | loopItem | caseItem | semiWrap | semi).m
  def semiWrap[$: P]: P[Node] = P(
    P(
      canExportItem | implItem | breakItem | continueItem | returnItem | assign,
    ).m ~/ semi.?,
  ).map((item, isSemi) => if isSemi.isEmpty then item else Semi(Some(item)))
  def semi[$: P] = P(";").map(_ => Semi(None))
  def canExportItem[$: P]: P[Node] =
    (word("pub") | word("private")).? ~ P(
      defItem | valItem | varItem | typeItem | classItems | importItem,
    )
  def primaryExpr[$: P] =
    P(tmplLit | ident | parens | paramsLit | literal | braces).m
  def factor[$: P]: P[Node] = P(unary | primaryExpr.flatMapX(factorR))

  // Braces/Args/Params
  def argsCt[$: P] = P("[" ~/ arg.rep(sep = ",") ~ "]")
  def args[$: P] = P("(" ~/ arg.rep(sep = ",") ~ ")")
  def paramsLit[$: P] = params.map(ParamsLit.apply)
  def params[$: P] =
    (P(brack.map(_.flatten) | paramsRt).rep(1)).map(_.flatten.toList)
  def paramsRt[$: P] =
    P("(" ~/ P((selfSugar | param).rep(sep = ",")) ~ ")")
  def braces[$: P]: P[Node] =
    P("{" ~/ term.rep.map(_.toList) ~ "}").map(body => {
      // check if all terms are cases
      var caseItems = List.empty[Case]
      var anyNotCase = false
      body.foreach {
        case c: Case    => caseItems = caseItems :+ c
        case Semi(None) =>
        case _          => anyNotCase = true
      }
      if anyNotCase || body.isEmpty then Block(body)
      else CaseBlock(caseItems)
    })
  def brack[$: P] =
    P("[" ~/ P(introTy | constraint.map(e => Seq(e))).rep(sep = ",") ~ "]")
  def parens[$: P] = P(
    "(" ~/ arg.rep(sep = ",") ~ ("," ~ &(")")).map(_ => true).? ~ ")",
  ).map(p => {
    if p._2.isEmpty && p._1.size == 1 && !p._1.head.isInstanceOf[KeyedArg] then
      p._1.head
    else ArgsLit(p._1.toList)
  })
  def arg[$: P] = P(spread | keyedArg).m
  def spread[$: P]: P[Node] = P(".." ~/ arg).map(UnOp("..", _))
  def keyedArg[$: P] = P((compound ~ (":" ~/ compound).?).map {
    case (lhs, Some(rhs)) => KeyedArg(lhs, rhs)
    case (lhs, None)      => lhs
  })
  def introTy[$: P] =
    P(ident.rep(1, sep = " ") ~ typeAnnotation.? ~ &("," | "]")).map((e, t) =>
      e.map(Param(_, t.orElse(Some(Ident("Type"))), None, true)),
    )
  def constraint[$: P] =
    compare.map(e => Param(Ident("-"), Some(e), None, true)).m
  def param[$: P] =
    P(ident ~ typeAnnotation.? ~ initExpression.?).map(Param(_, _, _, false)).m
  def selfSugar[$: P] = P(
    chk("&") ~ chk(word("mut")) ~ word("self").!.map(Ident.apply).m ~ &(
      "," | ")",
    ),
  ).map((isRef, isMut, self) => {
    val ty1 = Ident("Self")
    val decorated = (isRef, isMut) match {
      case (true, true)  => Apply(Ident("RefMut"), List(ty1), true)
      case (true, false) => Apply(Ident("Ref"), List(ty1), true)
      case _             => ty1
    }
    Param(self, Some(decorated), None, false)
  }).m
  def chk[$: P](s: => P[Unit]) = P(s.?.!).map(_.nonEmpty)

  // Expressions
  def literal[$: P] = P(numberLit | booleanLit | stringLit | todoLit).m
  def ident[$: P] = id.map(Ident.apply).m
  def defItem[$: P] = P(sigItem("def") ~ typeAnnotation.? ~ initExpression.?)
    .map(Def.apply.tupled)
  def classItems[$: P] = classItem("class", false) | classItem("trait", true)
  def classItem[$: P](kw: String, abc: Boolean) =
    P(sigItem(kw) ~ termU).map(Class(_, _, _, abc))
  def implItem[$: P] = P(
    word("impl") ~ params.? ~/ factor ~/ (word("for") ~/ factor).? ~ braces,
  ).map {
    case (params, lhs, Some(rhs), body) => Impl(rhs, Some(lhs), params, body)
    case (params, lhs, None, body)      => Impl(lhs, None, params, body)
  }.m
  def sigItem[$: P](kw: String) = P(word(kw) ~/ ident ~ params.?)
  def valItem[$: P] = P(varLike("val")).map(Val.apply.tupled)
  def varItem[$: P] = P(varLike("var")).map(Var.apply.tupled)
  def typeItem[$: P] = P(varLike("type")).map(Typ.apply.tupled)
  def varLike[$: P](kw: String) =
    P(word(kw) ~/ ident ~ typeAnnotation.? ~ initExpression.?)
  def loopItem[$: P] = P(word("loop") ~/ braces).map(Loop.apply)
  def importItem[$: P] =
    P(word("import") ~/ termU ~ (word("from") ~/ termU).?)
      .map {
        case (path, None)       => Import(path, None)
        case (dest, Some(path)) => Import(path, Some(dest))
      }
  def forItem[$: P] = P(
    word("for") ~ "(" ~/ ident ~ word("in") ~ term ~ ")" ~ braces,
  ).map(For.apply.tupled)
  def whileItem[$: P] = P(
    word("while") ~/ parens ~ braces,
  ).map(While.apply)
  def breakItem[$: P] = P(word("break")).map(_ => Break())
  def continueItem[$: P] = P(word("continue")).map(_ => Continue())
  def ifItem[$: P]: P[Node] = P(
    word("if") ~/ parens ~ braces
      ~ (word("else") ~ P(ifItem | braces)).?,
  ).map(If.apply.tupled)
  def returnItem[$: P] = P(word("return") ~/ termU).map(Return.apply)
  // (Top) Compound expressions
  def compound[$: P] = P(
    andOr ~ (P(word("match").! ~/ braces) | P(word("as").! ~/ factor)).rep,
  ).map { case (lhs, rhs) =>
    rhs.foldLeft(lhs) {
      case (lhs, ("match", rhs)) => Match(lhs, rhs)
      case (lhs, ("as", rhs))    => As(lhs, rhs)
      case _                     => ???
    }
  }
  def moreAssigns[$: P] =
    "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>="
  def assign[$: P]: P[Node] = binOp(P(P("=" ~ !">") | moreAssigns), compound)
  def binOp[$: P](op: => P[Unit], next: => P[Node]): P[Node] =
    P(next ~ (op.! ~/ next).rep).map { case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) => BinOp(op, lhs, rhs) }
    }
  def andOr[$: P]: P[Node] = binOp(P("and" | "or"), compare)
  def relOp[$: P] = "<=" | ">=" | "==" | "!=" | P("<" ~ !"<") | P(">" ~ !">")
  def relTypeOp[$: P] = "<:" | ">:" | "=:"
  def inNotIn[$: P] = word("in") | (word("not") ~ word("in"))
  def compare[$: P]: P[Node] = binOp(P(relTypeOp | relOp | inNotIn), range_lit)
  def range_lit[$: P]: P[Node] = binOp(P(".."), bitOr)
  def bitOr[$: P]: P[Node] = binOp(CharIn("|") ~~ !"=", bitAnd)
  def bitAnd[$: P]: P[Node] = binOp(CharIn("&") ~~ !"=", bitShift)
  def bitShift[$: P]: P[Node] = binOp(P(("<<" | ">>") ~~ !"="), addSub)
  def addSub[$: P]: P[Node] = binOp(CharIn("+\\-") ~~ !"=", divMul)
  def divMul[$: P]: P[Node] = binOp(CharIn("*/") ~~ !"=", arithMod)
  def arithMod[$: P]: P[Node] = binOp(CharIn("%") ~~ !"=", factor)
  def unaryOps[$: P] = "!" | "~" | "-" | "+" | "&" | "*" | word("mut")
  def unary[$: P]: P[Node] = P(unaryOps.! ~ factor).map(UnOp.apply.tupled)
  def eBinR[$: P](e: Node) =
    question(e) | select(e) | applyItemCt(e) | applyItem(e) | lambda(e)
  def factorR[$: P](e: Node): P[Node] =
    P(("" ~ eBinR(e)).flatMapX(factorR) | P("").map(_ => e))
  def question[$: P](lhs: Node) = "?".!.map(op => UnOp(op, lhs))
  def select[$: P](lhs: Node) =
    P(("." | "::").! ~ ident).map((op, rhs) => Select(lhs, rhs, op != "."))
  def applyItemCt[$: P](lhs: Node) =
    argsCt.map(rhs => Apply(lhs, rhs.toList, true))
  def applyItem[$: P](lhs: Node) =
    args.map(rhs => Apply(lhs, rhs.toList, false))
  def lambda[$: P](lhs: Node) = P("=>" ~/ compound).map(rhs => Lambda(lhs, rhs))
  def caseItem[$: P] = P("case" ~/ factor).map {
    case Lambda(lhs, rhs) => Case(lhs, Some(rhs))
    case x                => Case(x, None)
  }
  def typeAnnotation[$: P] = P(":" ~/ factor).m
  def initExpression[$: P] = P("=" ~/ term).m

  // Weak Keywords: from
  // Strong Keywords
  val keywords =
    Set(
      // to reduce js load time a bit
      "pub|private|impl|yield|lazy|as|import|module|unsafe|match|implicit|break|continue|using|throw|return|case|def|class|trait|type|if|else|for|while|loop|val|var|and|or|in|not|mut|true|false"
        .split('|')*,
    )
}

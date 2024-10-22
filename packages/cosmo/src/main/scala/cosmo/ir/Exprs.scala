package cosmo.ir

object Exprs {
  import cosmo.DefInfo
  class Term {}
  class Expr {}
  type T = Term;
  type E = Expr;
  // region: Exprs
  final case class Opaque(expr: Option[String], stmt: Option[String])
      extends E {}
  object Opaque {
    val empty = Opaque(None, None)
    def expr(expr: String) = Opaque(Some(expr), None)
    def stmt(stmt: String) = Opaque(None, Some(stmt))
  }
  final case class Region(stmts: List[T], semi: Boolean) extends E {
    override def toString: String = stmts.mkString("Region{ ", "; ", " }")
  }
  final case class Loop(body: T) extends E {}
  final case class While(cond: T, body: T) extends E {}
  final case class For(name: T, iter: T, body: T) extends E {}
  final case class Break() extends E {}
  final case class Continue() extends E {}
  final case class Return(value: T) extends E {}
  final case class If(cond: T, cont_bb: T, else_bb: Option[T]) extends E {}
  final case class As(lhs: T, rhs: T) extends E {}
  final case class UnOp(op: String, lhs: T) extends E {}
  final case class BinOp(op: String, lhs: T, rhs: T) extends E {}
  final case class BinInst(op: BinInstOp, lhs: T, rhs: T) extends E {}
  final case class KeyedArg(key: T, value: T) extends E {}
  final case class Apply(lhs: T, rhs: List[T]) extends E {
    override def toString: String = s"$lhs(${rhs.mkString(", ")})"
  }
  final case class TmplApply(
      lhs: T,
      strings: List[String],
      rhs: List[(T, Option[String])],
  ) extends E {
    override def toString: String =
      s"$lhs(${strings.mkString(", ")})(${rhs.mkString(", ")})"
  }
  final case class SelectExpr(lhs: T, rhs: String) extends E {
    override def toString: String = s"$lhs.$rhs"
  }
  final case class DestructExpr(dst: T, src: T) extends E {
    override def toString: String = s"$dst = $src"
  }
  final case class CaseExpr(cond: T, body: Option[T]) extends E {}
  final case class CaseRegion(cases: List[CaseExpr]) extends E {}
  final case class MatchExpr(lhs: T, body: CaseRegion) extends E {}
  final case class TupleLit(elems: Array[T]) extends E {
    override def toString: String = elems.mkString("tup(", ", ", ")")
  }
  // endregion: Exprs
}

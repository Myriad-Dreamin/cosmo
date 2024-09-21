package cosmo.syntax

import cosmo.DefId
import fastparse._

private type N = Ident
private type Str = String
private type Pol = Option[List[Param]]
private[cosmo] type No = Option[Node]
type TmplExp = Option[(Node, Option[String])]

sealed abstract class Node {
  var offset: Int = -1;
  var end: Int = -1;

  def children: Iterator[Node] = this match {
    case Decorate(lhs, rhs)  => Iterator(lhs, rhs)
    case Semi(semi)          => semi.iterator
    case Ident(name)         => Iterator.empty
    case BoolLit(value)      => Iterator.empty
    case IntLit(value)       => Iterator.empty
    case FloatLit(value)     => Iterator.empty
    case StringLit(value)    => Iterator.empty
    case ArgsLit(values)     => values.iterator
    case Block(stmts)        => stmts.iterator
    case CaseBlock(stmts)    => stmts.iterator
    case Val(name, ty, init) => Iterator(name) ++ ty.iterator ++ init.iterator
    case Var(name, ty, init) => Iterator(name) ++ ty.iterator ++ init.iterator
    case Typ(name, ty, init) => Iterator(name) ++ ty.iterator ++ init.iterator
    case Param(name, ty, init, _) =>
      Iterator(name) ++ ty.iterator ++ init.iterator
    case Import(path, dest) => Iterator(path) ++ dest.iterator
    case Class(name, ps, body, _) =>
      Iterator(name) ++ ps.iterator.flatten ++ Iterator(body)
    case Impl(rhs, lhs, ps, body) =>
      Iterator(rhs) ++ lhs.iterator ++ ps.iterator.flatten ++ Iterator(body)
    case Def(name, ps, ret, rhs) =>
      Iterator(name) ++ ps.iterator.flatten ++ ret.iterator ++ rhs.iterator
    case Loop(body)            => Iterator(body)
    case While(cond, body)     => Iterator(cond, body)
    case For(name, iter, body) => Iterator(name, iter, body)
    case If(cond, cont_bb, else_bb) =>
      Iterator(cond, cont_bb) ++ else_bb.iterator
    case Break()             => Iterator.empty
    case Continue()          => Iterator.empty
    case Return(value)       => Iterator(value)
    case UnOp(op, lhs)       => Iterator(lhs)
    case BinOp(op, lhs, rhs) => Iterator(lhs, rhs)
    case Match(lhs, rhs)     => Iterator(lhs, rhs)
    case As(lhs, rhs)        => Iterator(lhs, rhs)
    case Select(lhs, rhs, _) => Iterator(lhs, rhs)
    case Apply(lhs, rhs)     => Iterator(lhs) ++ rhs.iterator
    case TmplApply(lhs, rhs) =>
      Iterator(lhs) ++ rhs.iterator.flatMap(_._2.map(_._1))
    case KeyedArg(key, value) => Iterator(key, value)
    case Case(cond, body)     => Iterator(cond) ++ body.iterator
    case TodoLit              => Iterator.empty
  }
}

object NodeParse {
  implicit class Mapper[T <: Node](n: => P[T])(implicit ctx: P[_]) {
    def m = {
      val l = ctx.index;
      val r = n.map(node => { node.offset = l; node.end = ctx.index; node });
      ctx.asInstanceOf[P[T]]
    }
  }
}

// Kind: Decorators
// A node that is terminated by a semicolon
final case class Semi(semi: No) extends Node
// A node that is decorated by another node
final case class Decorate(lhs: Node, rhs: Node) extends Node

// Kind: Literals
// Just panic on problematic impls
object TodoLit extends Node
// Identifier
final case class Ident(name: Str) extends Node
// Boolean Literal
final case class BoolLit(value: Boolean) extends Node
// Integer Literal
final case class IntLit(value: BigInt) extends Node
// Float Literal
final case class FloatLit(value: BigDecimal) extends Node
// String Literal
final case class StringLit(value: Str) extends Node
// Argument Literal (Named and Nameless Tuples)
final case class ArgsLit(values: List[Node]) extends Node

// Kind: Blocks
// A block of statements
final case class Block(stmts: List[Node]) extends Node
// A block bspecially for match blocks
final case class CaseBlock(stmts: List[Case]) extends Node
// Kind: Var Decls
// constant level-0 variable
final case class Val(name: N, ty: No, init: No) extends Node
// mutable  level-0 variable
final case class Var(name: N, ty: No, init: No) extends Node
// constant level-1 variable
final case class Typ(name: N, ty: No, init: No) extends Node
// mutable  level-0 parameter (ct: must evaluated at compile-time)
final case class Param(name: N, ty: No, init: No, ct: Boolean) extends Node
// `import dest from path`
final case class Import(path: Node, dest: No) extends Node
// Kind: Def Decls
// ab = true:  `trait name(params) body`
// ab = false: `class name(params) body`
final case class Class(name: N, ps: Pol, body: Node, ab: Boolean) extends Node
// Impl Definition
// Either: `impl rhs {}`
// Or:     `impl lhs for rhs {}`
final case class Impl(rhs: Node, lhs: No, params: Pol, body: Node) extends Node
final case class Def(name: N, params: Pol, ret: No, rhs: No) extends Node
// Kind: Control Flow
final case class Loop(body: Node) extends Node
final case class While(cond: Node, body: Node) extends Node
final case class For(name: N, iter: Node, body: Node) extends Node
final case class If(cond: Node, cont_bb: Node, else_bb: No) extends Node
final case class Break() extends Node
final case class Continue() extends Node
final case class Return(value: Node) extends Node
// Kind: Expressions
final case class UnOp(op: Str, lhs: Node) extends Node
final case class BinOp(op: Str, lhs: Node, rhs: Node) extends Node
final case class Match(lhs: Node, rhs: Node) extends Node
final case class As(lhs: Node, rhs: Node) extends Node
final case class Select(lhs: Node, rhs: Ident, ct: Boolean) extends Node
final case class Apply(lhs: Node, rhs: List[Node]) extends Node
final case class TmplApply(lhs: Node, rhs: List[(String, TmplExp)]) extends Node
final case class KeyedArg(key: Node, value: Node) extends Node
// Kind: Clauses
final case class Case(cond: Node, body: No) extends Node {
  def isWildcard: Boolean = cond match {
    case Ident("_") => true; case _ => false
  }
}

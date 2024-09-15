package cosmo.syntax

import cosmo.DefId

private type Str = String
private type Pol = Option[List[Param]]
private type No = Option[Node]
type TmplExp = Option[(Node, Option[String])]

sealed abstract class Node
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
final case class Val(name: Str, ty: No, init: No) extends Node
// mutable  level-0 variable
final case class Var(name: Str, ty: No, init: No) extends Node
// constant level-1 variable
final case class Typ(name: Str, ty: No, init: No) extends Node
// mutable  level-0 parameter (ct: must evaluated at compile-time)
final case class Param(name: Str, ty: No, init: No, ct: Boolean) extends Node
// `import dest from path`
final case class Import(path: Node, dest: No) extends Node
// Kind: Def Decls
// ab = true:  `trait name(params) body`
// ab = false: `class name(params) body`
final case class Class(name: Str, ps: Pol, body: Node, ab: Boolean) extends Node
// Impl Definition
// Either: `impl rhs {}`
// Or:     `impl lhs for rhs {}`
final case class Impl(rhs: Node, lhs: No, params: Pol, body: Node) extends Node
final case class Def(name: Str, params: Pol, ret: No, rhs: No) extends Node
// Kind: Control Flow
final case class Loop(body: Node) extends Node
final case class While(cond: Node, body: Node) extends Node
final case class For(name: Str, iter: Node, body: Node) extends Node
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

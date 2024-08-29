package cosmo.syntax

import cosmo.DefId

private type Str = String
private type Pol = Option[List[Param]]
private type No = Option[Node]

sealed abstract class Node
object Self extends Node
object BigSelf extends Node
object TodoLit extends Node
final case class Semi(semi: No) extends Node
final case class Ident(name: Str) extends Node
final case class BoolLit(value: Boolean) extends Node
final case class IntLit(value: Int) extends Node
final case class StringLit(value: Str) extends Node
final case class Block(stmts: List[Node]) extends Node
final case class Val(name: Str, ty: No, init: No) extends Node
final case class Var(name: Str, ty: No, init: No) extends Node
final case class Class(name: Str, params: Pol, body: Node) extends Node
final case class Param(name: Str, ty: No, init: No) extends Node
final case class Def(name: Str, params: Pol, ret: No, rhs: No) extends Node
final case class Import(path: Node, dest: No) extends Node
final case class Loop(body: Node) extends Node
final case class For(name: Str, iter: Node, body: Node) extends Node
final case class Break() extends Node
final case class Continue() extends Node
final case class If(cond: Node, cont_bb: Node, else_bb: No) extends Node
final case class BinOp(op: Str, lhs: Node, rhs: Node) extends Node
final case class Match(lhs: Node, rhs: Node) extends Node
final case class As(lhs: Node, rhs: Node) extends Node
final case class Select(lhs: Node, rhs: Ident) extends Node
final case class Apply(lhs: Node, rhs: List[Node]) extends Node
final case class TmplApply(lhs: Node, rhs: List[(String, Option[(Node, Option[String])])])
    extends Node
final case class KeyedArg(key: Str, value: Node) extends Node
final case class Return(value: Node) extends Node
final case class CaseBlock(stmts: List[Case]) extends Node
final case class Case(cond: Node, body: No) extends Node

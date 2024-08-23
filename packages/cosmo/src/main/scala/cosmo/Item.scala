package cosmo.ir

import cosmo.{DefId, Type}

sealed abstract class Item
object NoneItem extends Item
object SelfItem extends Item
object Runtime extends Item
final case class Lit(value: Int) extends Item
final case class Opaque(value: String) extends Item
final case class Param(name: String, id: DefId, ty: Type) extends Item
final case class Def(id: DefId) extends Item
final case class Var(id: DefId, init: Item, isContant: Boolean) extends Item
final case class Variable(id: DefId) extends Item
final case class BinOp(op: String, lhs: Item, rhs: Item) extends Item
final case class Return(value: Item) extends Item
final case class Apply(lhs: Item, rhs: List[Item]) extends Item
final case class Select(lhs: Item, rhs: String) extends Item
final case class Match(lhs: Item, rhs: Item) extends Item
final case class Case(cond: Item, body: Item) extends Item
final case class Loop(body: Item) extends Item
final case class For(name: String, iter: Item, body: Item) extends Item
final case class Break() extends Item
final case class Continue() extends Item
final case class If(cond: Item, cont_bb: Item, else_bb: Option[Item])
    extends Item
final case class Region(stmts: List[Item]) extends Item
final case class Fn(
    params: Option[List[Param]],
    body: Option[Item],
) extends Item
final case class Class(id: DefId, vars: List[Var], defs: List[Item])
    extends Item {}
object Class {
  lazy val empty = Class(DefId(0), List.empty, List.empty)
}
final case class EnumClass(
    id: DefId,
    variants: List[Def],
    default: Option[Item],
) extends Item

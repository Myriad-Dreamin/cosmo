package cosmo.syntax

sealed abstract class Node
final case class Ident(name: String) extends Node
final case class Literal(value: Int) extends Node
final case class Block(stmts: List[Node]) extends Node
final case class Val(name: String, rhs: Node) extends Node
final case class Var(name: String, rhs: Node) extends Node
final case class Param(name: String, ty: Option[Node], init: Option[Node])
    extends Node
final case class Def(name: String, params: List[Param], rhs: Node) extends Node
final case class BinOp(op: String, lhs: Node, rhs: Node) extends Node
final case class Apply(lhs: Node, rhs: List[Node]) extends Node
final case class Return(value: Node) extends Node

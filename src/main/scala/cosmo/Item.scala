package cosmo.ir

sealed abstract class Item
final case class Lit(value: Int) extends Item
final case class Opaque(value: String) extends Item
final case class Region(stmts: List[Item]) extends Item
final case class Fn(
    params: List[String],
    body: Option[Item],
) extends Item

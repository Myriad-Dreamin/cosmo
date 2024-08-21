package cosmo

sealed abstract class Type
case object TopTy extends Type
case object BottomTy extends Type
final case class IntegerTy(val width: Int, val isUnsigned: Boolean)
    extends Type {
  override def toString: String = s"${if (isUnsigned) "u" else "i"}$width"
}
object IntegerTy {
  def parse(s: String): Option[IntegerTy] = {
    val unsigned = s.startsWith("u")
    val width = s.stripPrefix("u").stripPrefix("i").toInt
    Some(new IntegerTy(width, unsigned))
  }
}

final case class TypeVariable(val nameHint: String, val defId: DefId)
    extends Type {
  override def toString: String = s"$nameHint:${defId.id}"
}

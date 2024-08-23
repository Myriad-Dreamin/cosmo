package cosmo

import cosmo.ir.Value

sealed abstract class Type {
  def level: Int = this match {
    case TopKind(lvl)    => lvl
    case BottomKind(lvl) => lvl
    case SelfKind(lvl)   => lvl
    case _               => 1
  }
  def instantiate(implicit level: Int, ctx: cosmo.Env): Type = {
    this match {
      case TypeVariable(_, id) =>
        val info = ctx.defs(id)
        val ty = info.upperBounds.find {
          case TypeVariable(_, id) => false
          case _                   => true
        }
        val ty2 = ty.map { ty =>
          if ty.level > level then ty.instantiate
          else ty
        }
        ty2.getOrElse(TopKind(level))
      case TopKind(lvl)    => TopKind((lvl - 1).max(level))
      case BottomKind(lvl) => BottomKind((lvl - 1).max(level))
      case SelfKind(lvl)   => SelfKind((lvl - 1).max(level))
      case ty              => ty
    }
  }
}
case class TopKind(val lvl: Int) extends Type
case class BottomKind(val lvl: Int) extends Type
case class SelfKind(val lvl: Int) extends Type
// TopTy
val TopTy = TopKind(1)
val BottomTy = BottomKind(1)
val SelfTy = SelfKind(1)
val UniverseTy = TopKind(2)
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

final case class ValueTy(val value: Value) extends Type {
  override def toString: String = value.toString
}
final case class TypeVariable(val nameHint: String, val defId: DefId)
    extends Type {
  override def toString: String = s"$nameHint:${defId.id}"
}
final case class CppType(val name: String, val ns: List[String]) extends Type {
  override def toString: String = s"cpp($repr)"
  def repr: String = (ns :+ name).mkString("::")
}
final case class CppInsType(val target: CppType, val arguments: List[String])
    extends Type {
  override def toString: String = s"cpp($repr)"
  def repr: String = target.repr + "<" + arguments.mkString(", ") + ">"
}

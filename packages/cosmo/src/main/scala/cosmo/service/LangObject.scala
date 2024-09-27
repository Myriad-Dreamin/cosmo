package cosmo.service
import cosmo.ir._
import scala.annotation._

class LangObject(val item: Item) {
  lazy val defInfo = item match {
    case i: Class        => Some(i.id)
    case i: Impl         => Some(i.id)
    case i: Ref          => Some(i.id)
    case i: Fn           => Some(i.id)
    case i: CModule      => Some(i.id)
    case i: NativeModule => Some(i.id)
    case i: Param        => Some(i.id)
    case i: Var          => Some(i.id)
    case i: InferVar     => Some(i.info)
    case _               => None
  }
  def name = defInfo.map(_.name).getOrElse("")
  def range = defInfo.flatMap(_.pos)
  lazy val pretty: String = {
    implicit def rec(item: Item): String = LangObject(item).pretty;
    @tailrec
    def go(i: Item): String =
      i match {
        case Ref(_, _, Some(v)) => go(v)
        case i: Var             => i.pretty
        case i: Class           => i.pretty
        case i: Fn              => i.pretty
        case i: Impl            => i.pretty
        case i: NativeModule    => i.pretty
        case i: CModule         => i.pretty
        case i: Param           => i.pretty
        case i                  => i.toString
      }
    go(item)
  }
}

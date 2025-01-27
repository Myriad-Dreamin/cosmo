package cosmo.service
import cosmo.ir._
import scala.annotation._

class LangObject(val item: Term | Expr) {
  lazy val defInfo = item match {
    case i: Class        => Some(i.id)
    case i: Impl         => Some(i.id)
    case i: Ref          => Some(i.id)
    case i: CModule      => Some(i.id)
    case i: NativeModule => Some(i.id)
    case i: Param        => Some(i.id)
    case i: InferVar     => Some(i.info)
    case _               => None
  }
  def name = defInfo.map(_.name).getOrElse("")
  def range = defInfo.flatMap(_.pos)
  lazy val pretty: String = ???
}

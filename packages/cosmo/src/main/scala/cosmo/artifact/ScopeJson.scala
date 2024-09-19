package cosmo.artifact

import cosmo._
import cosmo.ir._

class ScopeJson(val env: Env) {
  import env._

  lazy val toScopeJson: String = {
    val sb = new StringBuilder()
    sb.append("{\n")
    sb.append(
      """  "h": ["name","parent","scopeStart","scopeEnd","posStart","posEnd","file","type"],
""",
    )
    sb.append(
      s"""  "f": { "file": "${fid.getOrElse("-")}" }""",
    )

    for (info <- defs.filter(!_.isHidden)) {
      sb.append(",\n")
      val pos = info.pos.map(p => s"${p._1},${p._2}").getOrElse("-1,-1")
      val parent =
        defParents
          .get(info.id)
          .map { p =>
            Seq(p._1.map(_.id).getOrElse(-1), p._2._1, p._2._2)
          }
          .iterator
          .flatten
          .mkString(",")
      val ty = info.ty.toString();
      sb.append(
        s"""  "i:${info.id.id}": ["${info.name}",${parent},${pos},"${info.env.fid
            .getOrElse(
              "-",
            )}","$ty"]""",
      )
    }
    sb.append("\n}")
    sb.toString()
  }

}

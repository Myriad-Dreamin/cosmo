package cosmo.service

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSConverters._

import cosmo._

class CosmoService(c: Cosmo) {
  import c._

  @JSExportAll
  class LspRange(val start: Int, val end: Int)
  def range(r: (Int, Int)) = new LspRange(r._1, r._2)

  @JSExportAll
  class HoverResult(val content: String, val range: js.UndefOr[LspRange])
  @JSExport
  def hover(path: String, offset: Int): js.UndefOr[HoverResult] =
    val env = fileIdOf(path).flatMap(loadModule);
    val item = env.flatMap(_.findItem(offset)).map(_.langObj);
    val rng = item.flatMap(_.range).map(range)
    item.map { i =>
      HoverResult(
        s"""```cos
${i.pretty}
```""",
        rng.orUndefined,
      )
    }.orUndefined

}

package cosmo.system

import scala.scalajs.js
import scala.scalajs.js.annotation._

import cosmo.system.CosmoSystem;

@JSExportTopLevel("CosmoJsPhysicalSystem")
class JsPhysicalSystem() extends CosmoSystem {
  var proxyReadFile: js.Function1[String, String] = null
  var proxyReadDir: js.Function1[String, js.Array[String]] = null
  var proxyExists: js.Function1[String, Boolean] = null

  @JSExport
  def proxy(proxyObject: js.Dynamic) = {
    class ProxyObject extends js.Object {
      var readFile: js.UndefOr[js.Function] = _
      var readDir: js.UndefOr[js.Function] = _
      var exists: js.UndefOr[js.Function] = _
    }

    proxyObject.updateDynamic("onReload")(
      js.Any.fromFunction1((path: String) => {
        println(s"Reloading $path")
      }),
    )

    val proxy = proxyObject.asInstanceOf[ProxyObject]

    if (proxy.readFile.isDefined) {
      val f = proxy.readFile.get.bind(proxyObject)
      proxyReadFile = f.asInstanceOf[js.Function1[String, String]]
    }
    if (proxy.readDir.isDefined) {
      val f = proxy.readDir.get.bind(proxyObject)
      proxyReadDir = f.asInstanceOf[js.Function1[String, js.Array[String]]]
    }
    if (proxy.exists.isDefined) {
      val f = proxy.exists.get.bind(proxyObject)
      proxyExists = f.asInstanceOf[js.Function1[String, Boolean]]
    }
  }

  def readFile(path: String): String = {
    if (proxyReadFile != null) {
      return proxyReadFile(path)
    }

    cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
  }

  def readDir(path: String): List[String] = {
    if (proxyReadDir != null) {
      return proxyReadDir(path).toList
    }

    cosmo.NodeFs.readdirSync(path).asInstanceOf[js.Array[String]].toList
  }

  def mkdir(path: String): Unit = {
    cosmo.NodeFs.mkdirSync(path, js.Dynamic.literal(recursive = true))
  }

  def writeFile(path: String, content: String): Unit = {
    cosmo.NodeFs.writeFileSync(path, content)
  }

  def absPath(path: String): String = {
    cosmo.NodeFs.realpathSync(path).asInstanceOf[String]
  }

  def exists(path: String): Boolean = {
    if (proxyExists != null) {
      return proxyExists(path)
    }

    cosmo.NodeFs.existsSync(path).asInstanceOf[Boolean]
  }

  def unlink(path: String): Unit = {
    cosmo.NodeFs.unlinkSync(path)
  }
}

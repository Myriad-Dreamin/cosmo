package cosmo.system

import scala.scalajs.js

import cosmo.system.CosmoSystem;

case class JsPhysicalSystem() extends CosmoSystem {
  def readFile(path: String): String = {
    cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
  }

  def readDir(path: String): List[String] = {
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
}

package cosmo

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON

import cosmo.system.CosmoSystem
import cosmo.FileId

enum PackageMetaSource {
  case ProjectPath(path: String)
}

class PackageMeta(
    val namespace: String,
    val name: String,
    val path: String,
    val version: String,
) {}

class JsPackageMeta extends js.Object {
  var name: String = _
  var version: String = _
}

class Package(metaSource: PackageMetaSource, system: CosmoSystem) {
  private lazy val meta: PackageMeta = loadMeta
  lazy val sources: Map[String, Source] = loadSources

  def namespace: String = meta.namespace
  def name: String = meta.name
  def version: String = meta.version

  private def loadMeta: PackageMeta = {
    metaSource match {
      case PackageMetaSource.ProjectPath(path) =>
        val metaPath = path + "/cosmo.json"
        val meta = system.readFile(metaPath)
        val parsed = JSON.parse(meta).asInstanceOf[JsPackageMeta]
        val (ns, name) = parsePackageName(parsed.name)
        new PackageMeta(ns, name, path, parsed.version)
    }
  }

  private def loadSources: Map[String, Source] = {
    // println(s"loading sources for $namespace/$name")
    scanDir(system, this, meta.path + "/src", "")
  }

  override def toString: String = s"@$namespace/$name:$version"

  def destDir(releaseDir: String): String =
    releaseDir + "/" + namespace + "/" + name + "/src"
}
object Package {
  val any = new Package(PackageMetaSource.ProjectPath(""), null)
}

class Source(val fid: FileId, val source: String) {}

def parsePackageName(name: String): (String, String) = {
  val parts = name.split("/")
  if (parts.length == 1) {
    ("", parts(0))
  } else {
    if (!parts(0).startsWith("@")) {
      throw new Exception("package name must start with @")
    }
    (parts(0).substring(1), parts(1))
  }
}

def scanDir(
    system: CosmoSystem,
    pkg: Package,
    path: String,
    relPaths: String,
): Map[String, Source] = {
  var sources = Map[String, Source]()
  val files = system.readDir(path)
  files.foreach { file =>
    val fullPath = path + "/" + file
    val relPath = relPaths + "/" + file
    if (file.endsWith(".cos")) {
      val fid = FileId(pkg, relPath)
      val source = system.readFile(fullPath)
      sources = sources + (relPath -> new Source(fid, source))
    } else {
      sources = sources ++ scanDir(system, pkg, fullPath, relPath)
    }
  }
  sources
}

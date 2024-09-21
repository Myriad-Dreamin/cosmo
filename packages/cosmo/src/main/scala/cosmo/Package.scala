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
    val fsPath: String,
    val version: String,
    val root: Option[String],
) {}

class JsPackageMeta extends js.Object {
  var name: String = _
  var version: String = _
  var root: js.UndefOr[String] = _
}

class Package(metaSource: PackageMetaSource, system: CosmoSystem) {
  private lazy val meta: PackageMeta = loadMeta
  lazy val sources: Map[String, Source] = loadSources

  export meta._

  private def loadMeta: PackageMeta = {
    metaSource match {
      case PackageMetaSource.ProjectPath(path) =>
        val metaPath = path + "/cosmo.json"
        val meta = system.readFile(metaPath)
        val parsed = JSON.parse(meta).asInstanceOf[JsPackageMeta]
        val (ns, name) = parsePackageName(parsed.name)
        new PackageMeta(ns, name, path, parsed.version, parsed.root.toOption)
    }
  }

  private def loadSources: Map[String, Source] = {
    debugln(s"loading sources for $namespace/$name")
    val root = canoPath(meta.root.getOrElse("src"))
    root.headOption match {
      case Some('/') => throw new Exception("root path must be relative")
      case Some('.') => throw new Exception("root path must inside package")
      case _         => {}
    }
    scanDir(system, this, root)
  }

  override def toString: String = s"@$namespace/$name:$version"

  def destDir(releaseDir: String): String =
    releaseDir + "/" + namespace + "/" + name
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
    relPaths: String,
): Map[String, Source] = {
  import NodePath.join;
  var sources = Map[String, Source]()
  val files =
    try system.readDir(join(pkg.fsPath, relPaths))
    catch {
      case _: Throwable => return sources
    }
  files.foreach { file =>
    val relPath = relPaths + "/" + file
    if (file.endsWith(".cos")) {
      val fid = FileId(pkg, relPath)
      val source = system.readFile(join(pkg.fsPath, relPath))
      sources = sources + (relPath -> new Source(fid, source))
    } else if (!file.contains(".")) { // todo: is directory
      sources = sources ++ scanDir(system, pkg, relPath)
    }
  }
  sources
}

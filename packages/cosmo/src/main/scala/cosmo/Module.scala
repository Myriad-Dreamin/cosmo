package cosmo

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON

import ir._
import system.CosmoSystem

final class DefId(val id: Int) extends AnyVal {}

case class FileId(val pkg: Package, val path: String) {
  lazy val ns = calcNs
  def calcNs =
    val pns = List(pkg.namespace + "_" + pkg.name)
    val relPath = NodePath.relative(pkg.root.getOrElse("src"), path)
    val fns = canoPath(relPath).stripSuffix(".cos").split("/").toList
    pns ::: fns
  override def toString(): String = s"$pkg/$path"
  def stripPath = path.stripSuffix(".cos")
}
object FileId {
  def any = FileId(Package.any, "")
  // from string
  def fromString(s: String, pkg: String => Option[Package]): Option[FileId] = {
    val parts = s.split(":", 2)
    val parts2 = parts(1).split("/", 2)
    pkg(parts(0) + ":" + parts2(0)).map(FileId(_, "/" + canoPath(parts2(1))))
  }

  def fromPkg(pkg: Package, path: String): FileId = {
    FileId(pkg, "/" + canoPath(NodePath.relative(pkg.fsPath, path)))
  }
}

class DefInfo(
    val name: String,
    val namespaces: List[String],
    var id: DefId,
    var env: Env,
    var syntax: Expr = Opaque.empty.e,
    var ty: Type = TopTy,
    var impls: List[Impl] = List(),
    var pos: Option[(Int, Int)] = None,
    var noMangle: Boolean = false,
    var isVar: Boolean = false,
    var isPhantom: Boolean = false,
    var isTypeVar: Boolean = false,
    var inClass: Boolean = false,
    var isBuiltin: Boolean = false,
    var isDependent: Boolean = true,
    var isOverride: Boolean = false,
    var isVirtual: Boolean = false,
    var isHidden: Boolean = false,
    var isMut: Boolean = true,
) {
  def isTrait = isVirtual
  def defName(stem: Boolean = false): String = {
    if (noMangle) this.name
    else if (isVar || stem) this.nameStem(this.id.id)
    else this.fullMangledName(this.id.id)
  }
  def nameStem(disambiguator: Int) =
    if (noMangle) name
    else mangledName(disambiguator)
  def fullName(disambiguator: Int) =
    if (noMangle) name
    else fullMangledName(disambiguator)
  // todo: ${disambiguator}
  def mangledName(disambiguator: Int) = name
  def fullMangledName(disambiguator: Int) =
    val ens = env.fid.map(_.ns).getOrElse(List())
    ((ens ::: namespaces) :+ s"${name}").mkString("::")
  def value = env.items.get(id)
  def instantiateTy = ty // todo: instantiate type
  def mod = if isTypeVar then "type "
  else if isMut then "var "
  else "val "

  override def toString(): String =
    s"${defName(false)} at ${env.fid.map(_.stripPath).getOrElse("")}:${pos.getOrElse((0, 0))._1}"
}

object DefInfo {
  def just(id: Int, env: Env) = new DefInfo("", List(), DefId(id), env)
}

class ExprInfo(var id: DefId) {}
object ExprInfo {
  def empty = new ExprInfo(DefId(0))
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

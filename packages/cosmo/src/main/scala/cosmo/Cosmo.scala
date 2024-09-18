package cosmo

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation._
import fastparse.Parsed

import cosmo.system._
import cosmo.linker._
import cosmo.formatter.formatCode

@main
def CosmoMain() = {}

case class FileId(val pkg: Package, val path: String) {
  lazy val ns = calcNs
  def calcNs =
    val pns = List(pkg.namespace + "_" + pkg.name)
    val fns = path.stripPrefix("/").stripSuffix(".cos").split("/").toList
    pns ::: fns
}
object FileId {
  def any = FileId(Package.any, "")
}

trait PackageManager {
  def loadModule(path: syntax.Node): Option[(FileId, Env)]
}

trait Transpiler {
  def transpile(
      src: String,
      fid: Option[FileId] = None,
  ): Option[(String, Boolean)]
}

@JSExportTopLevel("Cosmo")
class Cosmo extends PackageManager, Transpiler {
  var packages: Map[String, Map[String, Package]] = Map()
  val system: CosmoSystem = new JsPhysicalSystem()
  val linker: Linker = new CmakeLinker(system)
  val envBase = new Env(None, this).builtins()

  @JSExport
  def loadPackageByPath(path: String): Unit = {
    loadPackage(PackageMetaSource.ProjectPath(path))
  }

  @JSExport
  def getExecutable(path: String): String = {
    val releaseDir = "target/cosmo/release"
    linker.compile(path, this, releaseDir).getOrElse("")
  }

  @JSExport
  def convert(src: String): String = {
    val fid = None
    transpile(src, fid).map(_._1).getOrElse("#if 0\nCompilation failed\n#endif")
  }

  @JSExport
  def preloadPackage(name: String): Unit = {
    val FileId(pkg, pathInPkg) = resolvePackage(libPath(name))
    val releaseDir = "target/cosmo/release"
    linker.assemblePkg(pkg, this, releaseDir)
  }

  @JSExport
  def parseAsJson(s: String): String = {
    parse(s).map(syntax.toJson).getOrElse("")
  }

  def transpile(src: String, fid: Option[FileId]): Option[(String, Boolean)] =
    loadModuleBySrc(createEnv(fid), src).flatMap(cppBackend)

  def cppBackend(e: Env): Option[(String, Boolean)] = {
    implicit val env = e
    val code = new CodeGen().gen()
    Some((formatCode(code), env.noCore))
  }

  def loadPackage(source: PackageMetaSource): Unit = {
    val package_ = new Package(source, system)
    val nsPackages = packages.getOrElse(package_.namespace, Map())
    packages =
      packages + (package_.namespace -> (nsPackages + (package_.name -> package_)))
  }

  def loadModuleByDotPath(s: String): Option[(FileId, Env)] = {
    loadModule(libPath(s))
  }

  def loadModule(path: syntax.Node): Option[(FileId, Env)] = {
    val fid = resolvePackage(path)
    val env = createEnv(Some(fid));
    if (fid._1.toString().startsWith("@cosmo/std:")) {
      env.noCore = true
    }
    // println(s"Loading module $fid")
    loadModuleBySrc(env, fid.pkg.sources(fid.path).source).map((fid, _))
  }

  def parse(s: String): Option[syntax.Block] = {
    Some(parseBase(s, "r")).map(_.asInstanceOf[syntax.Block])
  }

  def parseBase(src: String, kind: String): syntax.Node = {
    fastparse.parse(
      src,
      kind match {
        case "r" => Parser.root(_)
        case "f" => Parser.factor(_)
        case _   => throw new Exception("Invalid kind")
      },
    ) match {
      case Parsed.Success(ast, _) => ast
      case Parsed.Failure(_, index, extra) =>
        println(extra.trace().longAggregateMsg)
        println(src.slice(index, index + 40))
        throw new Exception("Parsing failed")
    }
  }

  def resolvePackage(path: syntax.Node): FileId = {
    var names = path match {
      case syntax.Select(target, nn, _) =>
        // collect all the names in the target
        var names = List[String]()
        var current = target
        while (current != null) {
          current match {
            case syntax.Select(target, nn, _) =>
              names = nn.name :: names
              current = target
            case syntax.Ident(name) =>
              names = name :: names
              current = null
            case _ => throw new Exception("Invalid path")
          }
        }

        names = names :+ nn.name
        names
      case syntax.Ident(name) =>
        List(name)
      case _ =>
        throw new Exception("Invalid path")
    }

    val mayNs = names(0)
    val (ns, name, dropped) = if (mayNs == "std") {
      ("cosmo", mayNs, 1)
    } else {
      (mayNs, names(1), 2)
    }
    val pkg = packages
      .get(ns)
      .flatMap(_.get(name))
      .getOrElse(throw new Exception(s"Package @$ns/$name not found"))

    val pathInPkg = names.drop(dropped) match {
      case List() => "/lib.cos"
      case paths  => paths.mkString("/", "/", ".cos")
    }

    FileId(pkg, pathInPkg)
  }

  def loadModuleBySrc(env: Env, src: String): Option[Env] = {
    env.eval(parse(src).get)
    for (error <- env.errors) {
      println(error)
    }

    Some(env)
  }

  def createEnv(fid: Option[FileId]): Env = {
    val env = new Env(fid, this)
    env.defAlloc = envBase.defAlloc
    env.scopes.scopes = envBase.scopes.scopes
    env.builtinClasses = envBase.builtinClasses
    env.items = envBase.items
    env
  }
}

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def existsSync(path: String): Boolean = js.native
  def readFileSync(path: String, encoding: String): js.Any = js.native
  def readdirSync(path: String): js.Any = js.native
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def writeFileSync(path: String, content: String): Unit = js.native
  def realpathSync(path: String): js.Any = js.native
  def unlinkSync(path: String): Unit = js.native
}

@js.native
@JSImport("path", JSImport.Namespace)
object NodePath extends js.Object {
  def join(paths: String*): String = js.native
  def resolve(path: String*): String = js.native
  def relative(from: String, to: String): String = js.native
  def dirname(path: String): String = js.native
  def basename(path: String): String = js.native
}

final class SpawnSyncResult extends js.Object {
  var pid: Int = _
  var status: js.UndefOr[Int] = _
  var stdout: Array[Byte] | String = _
  var stderr: Array[Byte] | String = _
  var error: js.Error = _
}

@js.native
@JSImport("child_process", JSImport.Namespace)
object NodeChildProcess extends js.Object {
  def execSync(command: String, options: js.Dynamic): js.Dynamic = js.native
  def spawnSync(
      command: String,
      args: js.Array[String],
      options: js.Dynamic,
  ): SpawnSyncResult = js.native
}

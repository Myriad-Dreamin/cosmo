package cosmo

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation._
import fastparse._
import fastparse.Parsed

import cosmo.system._
import cosmo.linker._

@main
def CosmoMain() = {}

@JSExportTopLevel("Cosmo")
class Cosmo {
  var packages: Map[String, Map[String, Package]] = Map()
  val system: CosmoSystem = new JsPhysicalSystem()
  val linker: Linker = new MsvcLinker(system)

  @JSExport
  def loadPackageByPath(path: String): Unit = {
    loadPackage(PackageMetaSource.ProjectPath(path))
  }

  @JSExport
  def getExecutable(path: String): String = {
    val releaseDir = "target/cosmo/release"
    linker.compile(path, mayConvert, releaseDir).getOrElse("")
  }

  @JSExport
  def convert(src: String): String = {
    mayConvert(src).getOrElse("#if 0\nCompilation failed\n#endif")
  }

  @JSExport
  def preloadPackage(name: String): Unit = {
    val (pkg, pathInPkg) = resolvePackage(parsePackagePath(name) match {
      case Some(path) => path
      case None       => throw new Exception("Invalid package path")
    })
    val releaseDir = "target/cosmo/release"
    linker.assemblePkg(pkg, mayConvert, releaseDir)
  }

  def mayConvert(src: String): Option[String] =
    loadModuleBySrc(src).map(cppBackend).flatten

  def cppBackend(env: Env): Option[String] = Some(new CodeGen(env).gen())

  def loadPackage(source: PackageMetaSource): Unit = {
    val package_ = new Package(source, system)
    val nsPackages = packages.getOrElse(package_.namespace, Map())
    packages =
      packages + (package_.namespace -> (nsPackages + (package_.name -> package_)))
  }

  def loadModuleByDotPath(s: String): Option[Env] = {
    loadModule(parsePackagePath(s) match {
      case Some(path) => path
      case None       => throw new Exception("Invalid package path")
    })
  }

  def loadModule(path: syntax.Node): Option[Env] = {
    val (pkg, pathInPkg) = resolvePackage(path)
    // println(s"Loading module $pathInPkg from package ${pkg.name}")
    loadModuleBySrc(pkg.sources(pathInPkg).source)
  }

  def parsePackagePath(s: String): Option[syntax.Node] = {
    Some(parse(s, Parser.factor(_)) match {
      case Parsed.Success(ast, _) => ast
      case Parsed.Failure(_, index, extra) =>
        println(extra.trace().longAggregateMsg)
        println(s.slice(index, index + 40))
        return None
    })
  }

  def resolvePackage(path: syntax.Node): (Package, String) = {
    var names = path match {
      case syntax.Select(target, nn) =>
        // collect all the names in the target
        var names = List[String]()
        var current = target
        while (current != null) {
          current match {
            case syntax.Select(target, nn) =>
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

    val pathInPkg = names
      .drop(dropped)
      .mkString("/", "/", ".cos")

    (pkg, pathInPkg)
  }

  def loadModuleBySrc(src: String): Option[Env] = {
    val parsed = parse(src, Parser.root(_), verboseFailures = true)
    val ast = parsed match {
      case Parsed.Success(ast, _) => ast
      case Parsed.Failure(_, index, extra) =>
        println(extra.trace().longAggregateMsg)
        println(src.slice(index, index + 40))
        return None
    }
    val env = new Env(packages).eval(ast)
    for (error <- env.errors) {
      println(error)
    }

    Some(env)
  }
}

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def existsSync(path: String): js.Any = js.native
  def readFileSync(path: String, encoding: String): js.Any = js.native
  def readdirSync(path: String): js.Any = js.native
  def mkdirSync(path: String, options: js.Any): Unit = js.native
  def writeFileSync(path: String, content: String): Unit = js.native
  def realpathSync(path: String): js.Any = js.native
  def unlinkSync(path: String): Unit = js.native
}

@js.native
@JSImport("child_process", JSImport.Namespace)
object NodeChildProcess extends js.Object {
  def spawnSync(command: String): js.Dynamic = js.native
  def execSync(command: String, options: js.Dynamic): js.Dynamic = js.native
}

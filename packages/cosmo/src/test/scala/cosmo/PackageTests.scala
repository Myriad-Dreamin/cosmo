package cosmo
import scala.scalajs.js

class PackageTest extends munit.FunSuite:
  def runTestOnDir(path: String, modules: List[String]) = {
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath(path));
    modules.foreach { module =>
      compiler.loadModuleByDotPath(module) match {
        case Some(_) => ()
        case None    => fail(s"failed to load module $module")
      }
    }
  }

  test("std") {
    runTestOnDir(
      "library/std",
      List(
        "std.str",
        "std.collections.vec",
        "std.collections.set",
        "std.collections.map",
        "std.json",
        "std.fs",
      ),
    )
  }
end PackageTest

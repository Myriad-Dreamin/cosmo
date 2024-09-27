package cosmo
import scala.scalajs.js

class PackageFileTest extends munit.FunSuite:
  def runTest(path: String, expected: Option[String]) = {
    var compiler = new Cosmo();
    var fid = compiler.fileIdOf(path).map(_.toString);
    assertEquals(fid, expected)
  }

  test("loader") {
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    val fid = compiler.loadModuleByDotPath("std.prelude.lang").get.fid;
    assertEquals(fid.toString, "@cosmo/std:0.0.0/src/prelude/lang.cos")
  }

  test("std/fileIdOf") {
    val x = "library/std/src/";
    val y = "@cosmo/std:0.0.0/src/";
    runTest(x + "prelude/lang.cos", Some(y + "prelude/lang.cos"))
    runTest(x + "str.cos", Some(y + "str.cos"))
    val abs = NodePath.resolve("library/std/src/");
    runTest(abs + "/prelude/lang.cos", Some(y + "prelude/lang.cos"))
    runTest(abs + "/str.cos", Some(y + "str.cos"))
  }
end PackageFileTest

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

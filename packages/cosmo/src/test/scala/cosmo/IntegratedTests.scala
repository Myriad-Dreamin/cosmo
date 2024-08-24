package cosmo
import scala.scalajs.js

class IntegratedStdTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    compiler.preloadPackage("std");

    println(compiler.getExecutable(path))
  }

  test("Hello World".only) {
    runTestOnFile("samples/HelloWorld/main.cos")
  }

  test("readFile".only) {
    runTestOnFile("samples/Io/readFile.cos")
  }
end IntegratedStdTest

package cosmo
import scala.scalajs.js

class SelfHostTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    var result = compiler.mayConvert(src)
    println(result)
  }

  test("CompileDriver/callNode") {
    runTestOnFile("samples/CompileDriver/callNode.cos")
  }
end SelfHostTest

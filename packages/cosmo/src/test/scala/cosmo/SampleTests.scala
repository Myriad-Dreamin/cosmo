package cosmo
import scala.scalajs.js

class SampleTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    var result = compiler.mayConvert(src)
    println(result)
  }

  test("HelloWorld") {
    runTestOnFile("samples/HelloWorld/main.cos")
  }
  test("TypeAnnotation/add") {
    runTestOnFile("samples/TypeAnnotation/add.cos")
  }
  test("Class/basic") {
    runTestOnFile("samples/Class/basic.cos")
  }
  test("Class/nat") {
    runTestOnFile("samples/Class/nat.cos")
  }
  test("Class/method") {
    runTestOnFile("samples/Class/method.cos")
  }
  test("Class/staticMethod") {
    runTestOnFile("samples/Class/staticMethod.cos")
  }
  test("ControlFlow/loop") {
    runTestOnFile("samples/ControlFlow/loop.cos")
  }
  test("ControlFlow/forIn") {
    runTestOnFile("samples/ControlFlow/forIn.cos")
  }
  test("ControlFlow/mainIf") {
    runTestOnFile("samples/ControlFlow/mainIf.cos")
  }
  test("Format/templateLit") {
    runTestOnFile("samples/Format/templateLit.cos")
  }
  test("Io/readFile") {
    runTestOnFile("samples/Io/readFile.cos")
  }
  test("Vec/push") {
    runTestOnFile("samples/Vec/push.cos")
  }
end SampleTest

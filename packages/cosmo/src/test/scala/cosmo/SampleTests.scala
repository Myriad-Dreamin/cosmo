package cosmo
import scala.scalajs.js

class SampleTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    var result = compiler.transpile(src)
    println(result)
  }

  test("HelloWorld") {
    runTestOnFile("samples/HelloWorld/main.cos")
  }
  test("Syntax/literal") {
    runTestOnFile("samples/Syntax/literal.cos")
  }
  test("TypeAnnotation/add") {
    runTestOnFile("samples/TypeAnnotation/add.cos")
  }
  test("Class/basic") {
    runTestOnFile("samples/Class/basic.cos")
  }
  test("Class/jsonValue") {
    runTestOnFile("samples/Class/jsonValue.cos")
  }
  test("Class/nat") {
    runTestOnFile("samples/Class/nat.cos")
  }
  test("Class/natCons") {
    runTestOnFile("samples/Class/natCons.cos")
  }
  test("Class/method") {
    runTestOnFile("samples/Class/method.cos")
  }
  test("Class/staticMethod") {
    runTestOnFile("samples/Class/staticMethod.cos")
  }
  test("Trait/empty") {
    runTestOnFile("samples/Trait/empty.cos")
  }
  test("Trait/resultProblem".only) {
    runTestOnFile("samples/Trait/resultProblem.cos")
  }
  test("Trait/formatter") {
    runTestOnFile("samples/Trait/formatter.cos")
  }
  test("Trait/formatter_t") {
    runTestOnFile("samples/Trait/formatter_t.cos")
  }
  test("Trait/display") {
    runTestOnFile("samples/Trait/display.cos")
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
  test("Pattern/natAdd") {
    runTestOnFile("samples/Pattern/natAdd.cos")
  }
  test("Pattern/option") {
    runTestOnFile("samples/Pattern/option.cos")
  }
  test("Pattern/result") {
    runTestOnFile("samples/Pattern/result.cos")
  }
  test("Io/readFile") {
    runTestOnFile("samples/Io/readFile.cos")
  }
  test("Vec/push") {
    runTestOnFile("samples/Vec/push.cos")
  }
  test("PythonTutorial/a_calc") {
    runTestOnFile("samples/PythonTutorial/a_calc.cos")
  }
end SampleTest

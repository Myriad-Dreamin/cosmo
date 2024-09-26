package cosmo
import scala.scalajs.js

class ParserTest extends TestBase {
  implicit val storePath: String = "packages/cosmo/snapshots/ParserTests/";

  def runTestOnFile(path: String) = {
    checkSnapshot(path, src => pprint.apply(compiler.parse(src)).plainText);
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
  test("ControlFlow/loop") {
    runTestOnFile("samples/ControlFlow/loop.cos")
  }
  test("ControlFlow/forIn") {
    runTestOnFile("samples/ControlFlow/forIn.cos")
  }
  test("ControlFlow/mainIf") {
    runTestOnFile("samples/ControlFlow/mainIf.cos")
  }
  test("Syntax/literal") {
    runTestOnFile("samples/Syntax/literal.cos")
  }
  test("Syntax/cf.syntax") {
    runTestOnFile("samples/Syntax/cf.syntax.cos")
  }
  test("Syntax/expr.syntax") {
    runTestOnFile("samples/Syntax/expr.syntax.cos")
  }
  test("Syntax/callExpr.syntax") {
    runTestOnFile("samples/Syntax/callExpr.syntax.cos")
  }
  test("Syntax/tmplLit.syntax") {
    runTestOnFile("samples/Syntax/tmplLit.syntax.cos")
  }
  test("Syntax/decl.syntax") {
    runTestOnFile("samples/Syntax/decl.syntax.cos")
  }
  test("Syntax/lambda.syntax") {
    runTestOnFile("samples/Syntax/lambda.syntax.cos")
  }
  test("Syntax/matchExpr.syntax") {
    runTestOnFile("samples/Syntax/matchExpr.syntax.cos")
  }
  test("Vec/push") {
    runTestOnFile("samples/Vec/push.cos")
  }
}

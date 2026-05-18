package cosmo
import scala.scalajs.js

class ParserTest extends TestBase {
  implicit val storePath: String = "packages/cosmo/snapshots/ParserTests/";

  def runTestOnFile(path: String) = {
    checkSnapshot(path, src => pprint.apply(compiler.parse(src)).plainText);
  }

  test("HelloWorld") {
    runTestOnFile("samples/legacy/HelloWorld/main.cos")
  }
  test("TypeAnnotation/add") {
    runTestOnFile("samples/legacy/TypeAnnotation/add.cos")
  }
  test("Class/basic") {
    runTestOnFile("samples/legacy/Class/basic.cos")
  }
  test("Class/nat") {
    runTestOnFile("samples/legacy/Class/nat.cos")
  }
  test("ControlFlow/loop") {
    runTestOnFile("samples/legacy/ControlFlow/loop.cos")
  }
  test("ControlFlow/forIn") {
    runTestOnFile("samples/legacy/ControlFlow/forIn.cos")
  }
  test("ControlFlow/mainIf") {
    runTestOnFile("samples/legacy/ControlFlow/mainIf.cos")
  }
  test("Syntax/literal") {
    runTestOnFile("samples/legacy/Syntax/literal.cos")
  }
  test("Syntax/cf.syntax") {
    runTestOnFile("samples/legacy/Syntax/cf.syntax.cos")
  }
  test("Syntax/expr.syntax") {
    runTestOnFile("samples/legacy/Syntax/expr.syntax.cos")
  }
  test("Syntax/callExpr.syntax") {
    runTestOnFile("samples/legacy/Syntax/callExpr.syntax.cos")
  }
  test("Syntax/tmplLit.syntax") {
    runTestOnFile("samples/legacy/Syntax/tmplLit.syntax.cos")
  }
  test("Syntax/decl.syntax") {
    runTestOnFile("samples/legacy/Syntax/decl.syntax.cos")
  }
  test("Syntax/lambda.syntax") {
    runTestOnFile("samples/legacy/Syntax/lambda.syntax.cos")
  }
  test("Syntax/matchExpr.syntax") {
    runTestOnFile("samples/legacy/Syntax/matchExpr.syntax.cos")
  }
  test("Vec/push") {
    runTestOnFile("samples/legacy/Vec/push.cos")
  }
}

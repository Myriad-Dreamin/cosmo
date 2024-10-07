package cosmo
import scala.scalajs.js

class TypedTest extends TestBase {
  implicit val storePath: String = "packages/cosmo/snapshots/TyperTests/";

  def runTestOnFile(path: String) = checkSnapshot(path, caseOn);
  def caseOn(content: String) = {
    implicit val env: Env = compiler.evaluate("@noCore();\n" + content).get
    env.module.toDoc.pretty
  }

  test("Class/basic") {
    runTestOnFile("fixtures/Type/decls/class.cos-ast")
  }
  test("Func/basic") {
    runTestOnFile("fixtures/Type/decls/func.cos-ast")
  }
}

package cosmo
import scala.scalajs.js

import munit.diff.Diffs

class ParserTest extends munit.FunSuite {
  def runTestOnFile(path: String) = {

    val updateSnapshot: Boolean = false;

    // read the file
    val src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    val compiler = new Cosmo();
    val result = compiler.parse(src)
    val snapshot = pprint.apply(result).plainText;

    val snapshotPath = "packages/cosmo/snapshots/ParserTests/" + path.slice(
      0,
      path.length - 4,
    ) + ".cos-ast"

    if (updateSnapshot) {
      val dirPath = snapshotPath.slice(0, snapshotPath.lastIndexOf("/"))
      cosmo.NodeFs.mkdirSync(dirPath, js.Dynamic.literal("recursive" -> true))
      cosmo.NodeFs.writeFileSync(
        snapshotPath,
        snapshot,
      )
    } else {
      var expected =
        cosmo.NodeFs.readFileSync(snapshotPath, "utf8").asInstanceOf[String]
      // assertEquals(result, expected)
      assertEquals(
        snapshot,
        expected,
        "Snapshot does not match: " + snapshotPath + "\n" + Diffs.unifiedDiff(
          snapshot,
          expected,
        ),
      )
    }
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
  test("Vec/push") {
    runTestOnFile("samples/Vec/push.cos")
  }
}

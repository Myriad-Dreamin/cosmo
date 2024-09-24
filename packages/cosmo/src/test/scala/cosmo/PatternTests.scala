package cosmo
import scala.scalajs.js

implicit val storePath: String = "packages/cosmo/snapshots/Types/";

class PatternTest extends TestBase:

  def runTestOnFile(path: String) = checkSnapshot(path, patternOn);
  def patternOn(content: String) = {
    val Array(defs, cases) = content.split("-----");
    implicit val env: Env = compiler.evaluate("@noCore();\n" + defs).get

    var snapshot = List[String]();
    for (case Array(x, y) <- cases.split("\n").map(_.split("match"))) {
      val (lhs, rhs) = (env.valTerm(expr(x)), expr(y));
      env.errors = List();
      val result = env.matchOne(lhs, rhs).toDoc.pretty;
      val errors = env.errors.mkString("\n");
      snapshot =
        snapshot :+ s"[${x.trim} match ${y.trim}] => $result, err: \"$errors\"";
    }

    snapshot.mkString("\n");
  }

  test("HelloWorld") {
    runTestOnFile("fixtures/Type/patterns/HelloWorld.cos-ast");
  }
  test("class") {
    runTestOnFile("fixtures/Type/patterns/class.cos-ast");
  }
  test("class2") {
    runTestOnFile("fixtures/Type/patterns/class2.cos-ast");
  }
  test("nat") {
    runTestOnFile("fixtures/Type/patterns/nat.cos-ast");
  }
end PatternTest

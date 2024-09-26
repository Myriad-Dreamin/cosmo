package cosmo
import scala.scalajs.js

implicit val storePath: String = "packages/cosmo/snapshots/Types/";

class CaseTest extends TestBase:

  def runTestOnFile(path: String) = checkSnapshot(path, caseOn);
  def caseOn(content: String) = {
    val Array(defs, cases) = content.split("-----");
    implicit val env: Env = compiler.evaluate("@noCore();\n" + defs).get

    var snapshot = List[String]();
    for (case Array(x, y) <- cases.split("\n").map(_.split("match"))) {
      val (lhs, rhsCase) =
        env.scopes.withScope((env.valTerm(expr(x)), expr("case " + y)));
      val rhs = rhsCase.asInstanceOf[ir.CaseExpr].cond;
      env.errors = List();
      val result = env.matchOne(lhs, rhs).toDoc.pretty;
      val errors = env.errors.mkString("\n");
      snapshot =
        snapshot :+ s"[${x.trim} match ${y.trim}] => $result, err: \"${escapeStr(errors)}\"";
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
  test("result") {
    runTestOnFile("fixtures/Type/patterns/result.cos-ast");
  }
end CaseTest

class MatchTest extends TestBase:

  def runTestOnFile(path: String) = checkSnapshot(path, matchOn);
  def matchOn(content: String) = {
    val Array(defs, cases) = content.split("----- -----");
    implicit val env: Env = compiler.evaluate("@noCore();\n" + defs).get

    var snapshot = List[String]();
    for (matchExpr <- cases.split("-----")) {
      env.errors = List();
      val t =
        env.scopes.withScope((env.valTerm(expr(matchExpr))));
      val result = t.toDoc.pretty;
      val errors = env.errors.mkString("\n");
      snapshot =
        snapshot :+ s"[${matchExpr.trim}] => $result, err: \"${escapeStr(errors)}\"";
    }

    snapshot.mkString("\n");
  }

  test("HelloWorld") {
    runTestOnFile("fixtures/Type/matches/HelloWorld.cos-ast");
  }
  test("string") {
    runTestOnFile("fixtures/Type/matches/string.cos-ast");
  }
  test("number") {
    runTestOnFile("fixtures/Type/matches/number.cos-ast");
  }
  test("class") {
    runTestOnFile("fixtures/Type/matches/class.cos-ast");
  }
  test("nat") {
    runTestOnFile("fixtures/Type/matches/nat.cos-ast");
  }
  test("result") {
    runTestOnFile("fixtures/Type/matches/result.cos-ast");
  }
  test("json") {
    runTestOnFile("fixtures/Type/matches/json.cos-ast");
  }
end MatchTest

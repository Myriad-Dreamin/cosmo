package cosmo

class SelectTest extends TestBase:

  def runTestOnFile(path: String) = checkSnapshot(path, caseOn);
  def caseOn(content: String) = {
    val Array(defs, cases) = content.split("-----");
    implicit val env: Env = compiler.evaluate("@noCore();\n" + defs).get

    var snapshot = List[String]();
    for (line <- cases.split("\n").map(_.trim).filter(_.nonEmpty)) {
      env.errors = List();
      val lhs = env.scopes.withScope(env.valTerm(expr(line)));
      val result = lhs.toDoc.pretty;
      val ty = env.tyOf(lhs).map(_.toDoc.pretty).getOrElse("?");
      val errors = env.errors.mkString("\n");
      snapshot =
        snapshot :+ s"[${line}] => $result, type: $ty, err: \"${escapeStr(errors)}\"";
    }

    snapshot.mkString("\n");
  }

  test("HelloWorld") {
    runTestOnFile("fixtures/Type/dispatches/HelloWorld.cos-ast");
  }
end SelectTest

package cosmo

import scala.scalajs.js
import munit.diff.Diffs

val syntaxOnly = false
val evalOnly = false
val updateSnapshot = false;

class TestBase extends munit.FunSuite:
  val compiler = new Cosmo();

  lazy val loadPackages = {
    if (!(syntaxOnly || evalOnly)) {
      compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    }
    true
  }

  def expr(x: String)(implicit env: Env) =
    env.expr(compiler.parse(x).get.stmts(0))

  def compilePath(path: String) = {
    loadPackages;
    // read the file
    val (content, env) = compiler.transpileByPath(path).get
    val item = if syntaxOnly then env.moduleAst else env.module
    if (syntaxOnly || evalOnly) {
      println(item.toDoc.pretty(showDef = true))
    } else {
      println(content)
    }
    env.report
  }

  def checkSnapshot(path: String, f: String => String)(implicit
      storePath: String,
  ) = {
    val content = cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String];
    val snapshot = f(content);
    val path2 = path.stripSuffix(".cos").stripSuffix(".cos-ast");
    val snapshotPath =
      NodePath.join(storePath, path2 + ".cos-ast")

    if (updateSnapshot) {
      val dirPath = NodePath.dirname(snapshotPath);
      NodeFs.mkdirSync(dirPath, js.Dynamic.literal("recursive" -> true))
      NodeFs.writeFileSync(
        snapshotPath,
        snapshot,
      )
    } else {
      var expected =
        NodeFs.readFileSync(snapshotPath, "utf8").asInstanceOf[String]
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
end TestBase

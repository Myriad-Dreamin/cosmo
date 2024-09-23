package cosmo

val syntaxOnly = false
val evalOnly = false

class TestBase extends munit.FunSuite:
  lazy val compiler = {
    var compiler = new Cosmo();
    if (!(syntaxOnly || evalOnly)) {
      compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    }
    compiler
  }

  def compilePath(path: String) = {
    // read the file
    var src = cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var (content, env) = compiler.transpile(src).get
    val item = if syntaxOnly then env.moduleAst else env.module
    if (syntaxOnly || evalOnly) {
      println(item.toDoc.pretty(showDef = true))
    } else {
      println(content)
    }
  }
end TestBase
